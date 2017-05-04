module Knuckleball.Types where

import Knuckleball.Import

import Network.TLS (Context, Information)

data Conn = Conn
    { connHandle :: Handle
    , connTLSCtx :: Context
    , connTLSInfo :: Information
    }
