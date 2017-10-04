module Knuckleball (main) where

import Knuckleball.Import

-- external modules

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.RawFilePath as B (withFile)
import qualified Data.Text.Encoding as T

-- local modules

import Knuckleball.Conf
import Knuckleball.Error
import Knuckleball.Network


data LoopSet = LoopSet
    { loopProcess :: Process CreatePipe CreatePipe Inherit
    , loopUpThread :: ThreadId
    , loopRestartKey :: ByteString
    }


main :: IO ()
main = do
    home <- fromMaybe errNoHome <$> getHomeDirectory
    Conf{..} <- fmap (either error id) $
        loadConf $ home <> "/.config/knuckleball/conf.yaml"
    upChan <- newChan :: IO (Chan ByteString)
    downChan <- newChan :: IO (Chan ByteString)
    connect cHost cPort $ \ conn -> do
        _ <- forkIO $ receiverNet conn downChan
        _ <- forkIO $ senderNet conn upChan

        _ <- readChan downChan
        writeChan upChan $ LB.toStrict $ B.toLazyByteString $ fold
            [ "USER ", b cUsername
            , " ", b cHostname
            , " ", b cHost
            , " :", b cRealname
            , "\r\nNICK :", b cNickname
            ]

        loopSet <- newLoopSet upChan
        mainLoop loopSet upChan downChan
  where
    b = B.byteString . T.encodeUtf8


upstream :: Handle -> Chan ByteString -> IO ()
upstream hdl chan = hIsClosed hdl >>= \ closed -> unless closed $ do
    msg <- B.hGetSome hdl 4096
    unless (B.null msg) $ if prefix `B.isPrefixOf` msg
        then writeChan chan (B.drop (B.length prefix) msg)
        else B.putStr ("Core says: " <> msg)
    upstream hdl chan
  where
    prefix = "NET "


newLoopSet :: Chan ByteString -> IO LoopSet
newLoopSet upChan = do
    process <- startProcess $ proc "knuckleball-core" []
        `setStdin` CreatePipe
        `setStdout` CreatePipe
    hSetBuffering (processStdin process) NoBuffering
    hSetBuffering (processStdout process) NoBuffering

    upThread <- forkIO $ upstream (processStdout process) upChan
    restartKey <- B.withFile "/dev/random" ReadMode $ \ h ->
        LB.toStrict . B.toLazyByteString . B.byteStringHex <$> B.hGetSome h 32

    print restartKey

    return LoopSet
        { loopProcess = process
        , loopUpThread = upThread
        , loopRestartKey = restartKey
        }


mainLoop :: LoopSet -> Chan ByteString -> Chan ByteString -> IO a
mainLoop loopSet@LoopSet{..} upChan downChan = do
    msg <- readChan downChan
    mainLoop' msg

  where
    mainLoop' msg
        | "PING " `B.isPrefixOf` msg = do
            writeChan upChan $ "PONG :" <> B.drop (B.length "PING :") msg
            continue
        | secondWord == "PRIVMSG" = if loopRestartKey `B.isSuffixOf` msg
            then do
                killThread loopUpThread
                _ <- stopProcess loopProcess
                nextloopSet <- newLoopSet upChan
                mainLoop nextloopSet upChan downChan
            else continue
        | otherwise = continue
      where
        continue = do
            B.hPut (processStdin loopProcess) ("NET " <> msg)
            mainLoop loopSet upChan downChan
        secondWord = B.takeWhile (/= ' ') $
            B.drop 1 $ B.dropWhile (/= ' ') msg
