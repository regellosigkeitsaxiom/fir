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
tryMatch_ state (e:es) (a:as) | e `elem` ("xX_"::String) = tryMatch_ ( max state Partial ) es as
                              | toLower e == toLower a = tryMatch_ state es as
                              | otherwise = NoMatch

data DotFir = DotFir
  { model :: String
  , linker :: String
  , options :: [ String ]
  } deriving ( Eq, Show )
instance FromJSON DotFir where
  parseJSON ( Object o ) = DotFir
    <$> o .: "model"
    <*> o .: "linker"
    <*> o .:? "options" .!= []
  parseJSON invalid = typeMismatch "Invalid .fir.yaml file" invalid
instance ToJSON DotFir where
  toJSON ( DotFir m l o ) = object
    [ "model" .= m
    , "linker" .= l
    , "options" .= o
    ]

showCurrentInfo :: IO ()
showCurrentInfo = getInfo >>= print

getInfo :: IO ( Either String DotFir )
getInfo = do
  que <- doesFileExist ".fir.yaml"
  if not que
  then return $ Left ".fir.yaml does not exist.\nMaybe you want to create one with `fir init`?"
  else do
    dotfir <- decodeEither <$> BS.readFile ".fir.yaml"
    case ( dotfir :: Either String DotFir ) of
      Left e -> return $ Left $ "Error while reading .fir.yaml file:\n" ++ e
      Right fir -> return dotfir
