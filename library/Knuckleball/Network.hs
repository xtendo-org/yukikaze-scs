module Knuckleball.Network where

import Knuckleball.Import

-- extra modules

import Network
import Network.TLS
import Network.TLS.Extra

-- local modules

import Knuckleball.Types

conn :: String -> Word16 -> IO (Maybe Conn)
conn host port = bracket open close $ \ (hdl, ctx) -> do
    info <- contextGetInformation ctx
    return $ fmap (Conn hdl ctx) info
  where
    open = do
        hdl <- connectTo host (PortNumber $ fromIntegral port)
        hSetBuffering hdl LineBuffering
        ctx <- contextNew hdl params
        handshake ctx
        return (hdl, ctx)
    defaultParams = defaultParamsClient host ""
    params = defaultParams
        { clientSupported = (clientSupported defaultParams)
            { supportedCiphers = ciphersuite_strong
            }
        , clientHooks = (clientHooks defaultParams)
            { onServerCertificate = \ _ _ _ _ -> return []
            }
        }
    close (hdl, ctx) = do
        contextClose ctx
        hClose hdl

