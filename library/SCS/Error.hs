module SCS.Error where

errNoHome :: a
errNoHome = error "No $HOME"

errConnFail :: a
errConnFail = error "TLS connection failed"
