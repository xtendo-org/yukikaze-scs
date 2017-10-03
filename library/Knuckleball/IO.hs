module Knuckleball.IO where

import Knuckleball.Import

-- external modules

import qualified Data.ByteString as B


sender :: (ByteString -> IO ()) -> ByteString -> Chan ByteString -> IO a
sender put sep chan = do
    payload <- (<> sep) <$> readChan chan
    put payload
    threadDelay (1000 * 1000)
    sender put sep chan


receiver :: IO ByteString -> ByteString -> Chan ByteString -> IO a
receiver get sep chan = receiver' ""
  where
    receiver' buffer = do
        (former, latter) <- loopTillRN buffer
        writeChan chan former
        receiver' latter
    loopTillRN buffer
        | B.null latter = do
            received <- get
            loopTillRN (buffer <> received)
        | otherwise = return
            ( B.take (B.length former + B.length sep) buffer
            , B.drop (B.length sep) latter
            )
      where
        (former, latter) = B.breakSubstring sep buffer


senderHdl :: Handle -> Chan ByteString -> IO a
senderHdl hdl = sender (B.hPut hdl) "\n"


receiverHdl :: Handle -> Chan ByteString -> IO a
receiverHdl hdl = receiver (B.hGet hdl 4096) "\n"
