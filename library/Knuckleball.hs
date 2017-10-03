module Knuckleball (main) where

import Knuckleball.Import

-- external modules

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Encoding as T

-- local modules

import Knuckleball.Conf
import Knuckleball.Error
import Knuckleball.Network
import Knuckleball.Types


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
        process <- startProcess $ proc "knucleball-core" []
            `setStdin` CreatePipe
            `setStdout` CreatePipe

        hSetBuffering (processStdin process) LineBuffering
        hSetBuffering (processStdout process) LineBuffering

        _ <- forkIO $ downstream downChan (processStdin process)
        _ <- forkIO $ upstream (processStdout process) upChan

        pong (Ctx upChan downChan)

  where
    b = B.byteString . T.encodeUtf8


pong :: Ctx -> IO ()
pong ctx@Ctx{..} = do
    msg <- readChan cDn
    when ("PING :" `B.isPrefixOf` msg) $
        writeChan cUp $
            "PONG :" <> B.drop (B.length "PING :") msg
    pong ctx


downstream :: Chan ByteString -> Handle -> IO a
downstream chan hdl = do
    msg <- readChan chan
    B.hPut hdl ("NET " <> msg)
    downstream chan hdl


upstream :: Handle -> Chan ByteString -> IO a
upstream hdl chan = do
    msg <- B.hGet hdl 4096
    if prefix `B.isPrefixOf` msg
    then writeChan chan (B.drop (B.length prefix) msg)
    else B.putStr ("Core says: " <> msg)
    upstream hdl chan
  where
    prefix = "NET "
