module SCS.Core where

import SCS.Import

-- external modules

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.RawFilePath as B (withFile)

-- local modules

import SCS.IO
import SCS.Types


data Core = Core
    { coreProcess :: Process CreatePipe CreatePipe Inherit
    , coreRecvThread :: ThreadId
    , coreRestartKey :: ByteString
    }


newCore :: Chan Event -> IO Core
newCore eventChan = do
    process <- startProcess $ proc "yukikaze" []
        `setStdin` CreatePipe
        `setStdout` CreatePipe
    hSetBuffering (processStdin process) NoBuffering
    hSetBuffering (processStdout process) NoBuffering

    recvThread <- forkIO $
        receiverHdl (processStdout process) $ writeChan eventChan . CoreEvent

    restartKey <- B.withFile "/dev/random" ReadMode $ \ h ->
        LB.toStrict . B.toLazyByteString . B.byteStringHex <$> B.hGetSome h 32

    return Core
        { coreProcess = process
        , coreRecvThread = recvThread
        , coreRestartKey = restartKey
        }
