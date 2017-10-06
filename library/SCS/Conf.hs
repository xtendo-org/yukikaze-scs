module SCS.Conf
    ( Conf(..)
    , loadConf
    ) where

import SCS.Import

-- extra modules

import Data.Aeson.TH
import Data.Yaml
import qualified Data.ByteString.RawFilePath as B

-- local modules

data Conf = Conf
    { cHost :: Text
    , cPort :: Word16
    , cHostname :: Text
    , cUsername :: Text
    , cRealname :: Text
    , cNickname :: Text
    } deriving Show

let
    snake :: String -> String
    snake [] = []
    snake (x : xs)
        | isUpper x = '_' : toLower x : snake xs
        | otherwise = x : snake xs
    headless :: String -> String
    headless s = case drop 1 s of
        [] -> []
        x : xs -> toLower x : xs
    m = snake . headless
  in
    deriveJSON defaultOptions { fieldLabelModifier = m } ''Conf

loadConf :: RawFilePath -> IO (Either String Conf)
loadConf path = decodeEither <$> B.readFile path
