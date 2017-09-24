{-# LANGUAGE OverloadedStrings #-}
module Info where

import Types
import Data.Yaml
import Data.Aeson.Types ( typeMismatch )
import Data.Char ( toLower )
import System.Directory ( doesFileExist )
import qualified Data.ByteString as BS

data Match = Full | Partial | NoMatch
  deriving ( Eq, Ord, Show )
tryMatch = tryMatch_ Full

tryMatch_ :: Match -> String -> String -> Match
tryMatch_ state [] [] = state
tryMatch_ state [] _ = max state Partial
tryMatch_ _ _ [] = NoMatch
tryMatch_ state (e:es) (a:as) | elem e ("xX_"::String) = tryMatch_ ( max state Partial ) es as
                              | toLower e == toLower a = tryMatch_ state es as
                              | otherwise = NoMatch

data DotFir = DotFir
  { model :: String
  , linker :: String
  } deriving ( Eq, Show )
instance FromJSON DotFir where
  parseJSON ( Object o ) = DotFir
    <$> o .: "model"
    <*> o .: "linker"
  parseJSON invalid = typeMismatch "Invalid .fir.yaml file" invalid
instance ToJSON DotFir where
  toJSON ( DotFir m l ) = object
    [ "model" .= m
    , "linker" .= l
    ]

showCurrentInfo :: IO ()
showCurrentInfo = getInfo >>= print

getInfo :: IO ( Either String DotFir )
getInfo = do
  que <- doesFileExist ".fir.yaml"
  case que of
    False -> return $ Left ".fir.yaml does not exist.\nMaybe you want to create one with `fir init`?"
    True -> do
      dotfir <- decodeEither <$> BS.readFile ".fir.yaml"
      case ( dotfir :: Either String DotFir ) of
        Left e -> return $ Left $ "Error while reading .fir.yaml file:\n" ++ e
        Right fir -> do
          return dotfir
