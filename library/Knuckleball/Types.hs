module Knuckleball.Types where

import Knuckleball.Import

import Network.TLS (Context, Information)

data Conn = Conn
    { connHandle :: Handle
    , connTLSCtx :: Context
    , connTLSInfo :: Information
    }

instance Show Conn where
    show Conn{..} = fold
        [ "{", show connHandle, ",", show connTLSInfo, "}" ]
