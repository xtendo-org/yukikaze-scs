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
import Knuckleball.Core
import Knuckleball.Error
import Knuckleball.Network
import Knuckleball.Types


main :: IO ()
main = do
    home <- fromMaybe errNoHome <$> getHomeDirectory
    Conf{..} <- fmap (either error id) $
        loadConf $ home <> "/.config/knuckleball/conf.yaml"
    upChan <- newChan :: IO (Chan ByteString)
    eventChan <- newChan :: IO (Chan Event)
    connect cHost cPort $ \ conn -> do
        _ <- forkIO $ receiverNet conn (writeChan eventChan . NetEvent)
        _ <- forkIO $ senderNet conn upChan

        _ <- readChan eventChan
        writeChan upChan $ LB.toStrict $ B.toLazyByteString $ fold
            [ "USER ", b cUsername
            , " ", b cHostname
            , " ", b cHost
            , " :", b cRealname
            , "\r\nNICK :", b cNickname
            ]

        core <- newCore eventChan
        B.withFile (home <> "/.config/knuckleball/restart_key") WriteMode $
            \ h -> B.hPut h (coreRestartKey core)
        mainLoop home core upChan eventChan
  where
    b = B.byteString . T.encodeUtf8


mainLoop :: ByteString -> Core -> Chan ByteString -> Chan Event -> IO a
mainLoop home core upChan eventChan = do
    event <- readChan eventChan
    case event of
        NetEvent msg -> netMsg msg
        CoreEvent msg -> if B.null msg
            then exitSuccess
            else do
                if prefix `B.isPrefixOf` msg
                then writeChan upChan (B.drop (B.length prefix) msg)
                else B.putStr ("Core says: " <> msg)
                mainLoop home core upChan eventChan
  where
    prefix = "NET "
    restartCore = do
        killThread (coreRecvThread core)
        _ <- stopProcess (coreProcess core)
        nextCore <- newCore eventChan
        B.withFile (home <> "/.config/knuckleball/restart_key") WriteMode $
            \ h -> B.hPut h (coreRestartKey nextCore)
        mainLoop home nextCore upChan eventChan
    netMsg msg
        | "PING " `B.isPrefixOf` msg = do
            writeChan upChan $ "PONG :" <> B.drop (B.length "PING :") msg
            continue
        | secondWord == "PRIVMSG"
        && coreRestartKey core `B.isInfixOf` msg = do
            B.putStr "Restarting ...\n"
            restartCore
        | otherwise = continue
      where
        continue = do
            B.hPut (processStdin $ coreProcess core) ("NET " <> msg <> "\n")
            mainLoop home core upChan eventChan
        secondWord = B.takeWhile (/= ' ') $
            B.drop 1 $ B.dropWhile (/= ' ') msg
