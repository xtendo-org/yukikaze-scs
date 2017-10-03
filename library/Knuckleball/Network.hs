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
import Knuckleball.IO
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


senderNet :: Conn -> Chan ByteString -> IO ()
senderNet Conn{..} = sender put "\r\n"
  where
    put b = do
        B.putStr (b <> "\n")
        TLS.sendData connTLSCtx (LB.fromStrict b)


receiverNet :: Conn -> Chan ByteString -> IO ()
receiverNet Conn{..} = receiver get "\r\n"
  where
    get = do
        msg <- TLS.recvData connTLSCtx
        B.putStr (msg <> "\n")
        return msg
