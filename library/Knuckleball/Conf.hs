module Knuckleball.Conf
    ( Conf(..)
    , loadConf
    ) where

import Knuckleball.Import

-- extra modules

import Data.Aeson.TH
import Data.Yaml
import qualified Data.ByteString.RawFilePath as B

-- local modules

data Conf = Conf
    { cfgHost :: Text
    , cfgPort :: Word16
    } deriving Show

let
    snake :: String -> String
    snake [] = []
    snake (x : xs)
        | isUpper x = '_' : toLower x : snake xs
        | otherwise = x : snake xs
    headless :: String -> String
    headless s = case drop 3 s of
        [] -> []
        x : xs -> toLower x : xs
    m = snake . headless
  in
    deriveJSON defaultOptions { fieldLabelModifier = m } ''Conf

loadConf :: RawFilePath -> IO (Either String Conf)
loadConf path = decodeEither <$> B.readFile path
