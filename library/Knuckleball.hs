module Knuckleball (main) where

import Knuckleball.Import

-- external modules

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

-- local modules

import Knuckleball.Conf
import Knuckleball.Error
import Knuckleball.Network
import Knuckleball.Types


main :: IO ()
main = do
    home <- fromMaybe errNoHome <$> getHomeDirectory
    Conf{..} <- fmap (either error id) $
        loadConf $ home <> "/.config/knuckleball/conf.yaml"
    backend <- newChan
    pingend <- dupChan backend
    connect cfgHost cfgPort $ \ conn -> do
        _ <- forkIO $ receiver conn backend
        _ <- forkIO $ pong conn pingend
        consumerInit conn backend


consumerInit :: Conn -> Chan ByteString -> IO ()
consumerInit conn chan = do
    _ <- readChan chan
    send conn "USER testme localhost irc.ozinger.org :testme\r\nNICK testme"
    loop
  where
    loop = readChan chan >> loop


pong :: Conn -> Chan ByteString -> IO ()
pong conn chan = do
    msg <- readChan chan
    when ("PING :" `B.isPrefixOf` msg) $
        send conn $ "PONG :" <> LB.fromStrict (B.drop (B.length "PING :") msg)
    pong conn chan
