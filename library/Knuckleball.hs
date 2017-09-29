module Knuckleball (main) where

import Knuckleball.Import

-- local modules

import Knuckleball.Conf
import Knuckleball.Error
import Knuckleball.Network


main :: IO ()
main = do
    home <- fromMaybe errNoHome <$> getHomeDirectory
    Conf{..} <- fmap (either error id) $
        loadConf $ home <> "/.config/knuckleball/conf.yaml"
    backend <- newChan
    connect cfgHost cfgPort $ \ conn -> do
        forkIO $ receiver conn backend
        readAndPrint backend


readAndPrint :: Chan ByteString -> IO ()
readAndPrint chan = do
    msg <- readChan chan
    print msg
    readAndPrint chan
