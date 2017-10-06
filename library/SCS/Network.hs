module SCS.Network where

import SCS.Import

-- extra modules

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Network as Net
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLS

-- local modules

import SCS.Error
import SCS.IO
import SCS.Types


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


senderNet :: Conn -> Chan ByteString -> IO a
senderNet Conn{..} = sender put "\r\n"
  where
    put b = do
        B.putStr b
        TLS.sendData connTLSCtx (LB.fromStrict b)


receiverNet :: Conn -> (ByteString -> IO ()) -> IO a
receiverNet Conn{..} = receiver "\r\n" get
  where
    get = do
        msg <- TLS.recvData connTLSCtx
        B.putStr msg
        return msg
