module Knuckleball.Network where

import Knuckleball.Import

-- extra modules

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Network as Net
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLS

-- local modules

import Knuckleball.Error
import Knuckleball.Types


connect :: Text -> Word16 -> (Conn -> IO a) -> IO a
connect tHost port action = bracket open close $ \ (hdl, ctx) -> do
    info <- TLS.contextGetInformation ctx
    action $ Conn hdl ctx $ fromMaybe errConnFail info
  where
    host = T.unpack tHost
    open = do
        hdl <- Net.connectTo host (Net.PortNumber $ fromIntegral port)
        hSetBuffering hdl NoBuffering
        ctx <- TLS.contextNew hdl params
        TLS.handshake ctx
        return (hdl, ctx)
    defaultParams = TLS.defaultParamsClient host ""
    params = defaultParams
        { TLS.clientSupported = (TLS.clientSupported defaultParams)
            { TLS.supportedCiphers = TLS.ciphersuite_strong
            }
        , TLS.clientHooks = (TLS.clientHooks defaultParams)
            { TLS.onServerCertificate = \ _ _ _ _ -> return []
            }
        }
    close (hdl, ctx) = do
        TLS.contextClose ctx
        hClose hdl


sender :: Conn -> Chan LB.ByteString -> IO ()
sender conn@Conn{..} chan = do
    payload <- (<> "\r\n") <$> readChan chan
    LB.putStr payload
    TLS.sendData connTLSCtx payload
    sender conn chan


receiver :: Conn -> Chan ByteString -> IO ()
receiver Conn{..} chan = receiver' ""
  where
    receiver' buffer = do
        (former, latter) <- loopTillRN buffer
        B.putStr former
        writeChan chan former
        receiver' latter
    loopTillRN buffer
        | B.null latter = do
            received <- get
            loopTillRN (buffer <> received)
        | otherwise = return
            (B.take (B.length former + 4) buffer, B.drop 4 latter)
      where
        (former, latter) = B.breakSubstring "\r\n" buffer
    get :: IO ByteString
    get = TLS.recvData connTLSCtx
