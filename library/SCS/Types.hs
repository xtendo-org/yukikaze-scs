module SCS.Types where

import SCS.Import

-- external modules

import Network.TLS (Context, Information)


data Conn = Conn
    { connHandle :: Handle
    , connTLSCtx :: Context
    , connTLSInfo :: Information
    }

instance Show Conn where
    show Conn{..} = fold
        [ "{", show connHandle, ",", show connTLSInfo, "}" ]


data Ctx = Ctx
    { cUp :: Chan ByteString
    , cDn :: Chan ByteString
    }


data Event
    = NetEvent ByteString
    | CoreEvent ByteString
