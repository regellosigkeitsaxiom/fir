{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.Yaml
import Data.Aeson.Types ( typeMismatch, Value )
import Data.Maybe ( maybeToList )

data FlashPoint
  = Local
    { command_fpl :: String
    , name_fpl :: String
    }
  | Remote
    { keyLocation :: String
    , port :: Int                     
    , command_fpr :: String
    , name_fpr :: String
    }
  deriving ( Eq, Show )

data Document = Document
  { code :: Maybe String
  , file :: FilePath
  , description :: String
  } deriving ( Eq, Show )
instance ToJSON Document where
  toJSON ( Document c f d ) = object
    [ "code" .= c
    , "file" .= f
    , "description" .= d
    ]
instance FromJSON Document where
  parseJSON ( Object o ) = Document
    <$> o .:? "code"
    <*> o .: "file"
    <*> o .: "description"
  parseJSON invalid = typeMismatch "Invalid format for document:\n" invalid

data Template = Template
  { descr :: String
  , archive :: FilePath
  } deriving ( Eq, Show )
instance ToJSON Template where
  toJSON ( Template d a ) = object
    [ "description" .= d
    , "archive" .= a
    ]
instance FromJSON Template where
  parseJSON ( Object o ) = Template
    <$> o .: "description"
    <*> o .: "archive"
  parseJSON invalid = typeMismatch "Invalid format for template:\n" invalid

data Core
  = CortexM0
  | CortexM0plus
  | CortexM3
  | CortexM4
  | CortexM7
  deriving ( Eq, Show )
instance ToJSON Core where
  toJSON CortexM0 = "M0"
  toJSON CortexM0plus = "M0+"
  toJSON CortexM3 = "M3"
  toJSON CortexM4 = "M4"
  toJSON CortexM7 = "M7"
instance FromJSON Core where
  parseJSON "m0" = pure CortexM0
  parseJSON "M0" = pure CortexM0
  parseJSON "m0+" = pure CortexM0plus
  parseJSON "M0+" = pure CortexM0plus
  parseJSON "m0plus" = pure CortexM0plus
  parseJSON "M0plus" = pure CortexM0plus
  parseJSON "m3" = pure CortexM3
  parseJSON "M3" = pure CortexM3
  parseJSON "m4" = pure CortexM4
  parseJSON "M4" = pure CortexM4
  parseJSON "m7" = pure CortexM7
  parseJSON "M7" = pure CortexM7
  parseJSON invalid = typeMismatch ( "Invalid core type: " ++ show invalid ) invalid

data Manuals = Manuals
  { reference :: [ Document ]
  , datasheet :: [ Document ]
  , errata :: [ Document ]
  , other :: [ Document ]
  } deriving ( Eq, Show )
instance ToJSON Manuals where
  toJSON x = object
    [ "reference" .= reference x
    , "datasheet" .= datasheet x
    , "other-documents" .= other x
    , "errata" .= errata x
    ]
instance FromJSON Manuals where
  parseJSON ( Object o ) = Manuals
    <$> o .: "reference"
    <*> o .: "datasheet"
    <*> o .: "errata"
    <*> o .: "other-documents"
  parseJSON invalid = typeMismatch "Invalid format for mcu description:\n" invalid

data MCU = MCU
  { manuals :: Manuals
  , fullname :: String
  , linkers :: [ FilePath ]
  , templates :: [ Template ]
  , helpText :: String
  , core :: Core
  , startup :: FilePath
  , cmsis :: [ FilePath ]
  , hal :: [ FilePath ]
  , define :: String
  } deriving ( Eq, Show )
instance ToJSON MCU where
  toJSON x = object
    [ "fullname" .= fullname x
    , "manuals" .= manuals x
    , "linkers" .= linkers x
    , "templates" .= templates x
    , "help" .= helpText x
    , "core" .= core x
    , "startup-code" .= startup x
    , "cmsis" .= cmsis x
    , "cmsis" .= hal x
    , "define" .= define x
    ]
instance FromJSON MCU where
  parseJSON ( Object o ) = MCU
    <$> o .: "manuals"
    <*> o .: "fullname"
    <*> o .: "linkers"
    <*> o .: "templates"
    <*> o .: "help"
    <*> o .: "core"
    <*> o .: "startup-code"
    <*> o .: "cmsis"
    <*> ( maybeToList <$> o .:? "hal" )
    <*> o .: "define"
  parseJSON invalid = typeMismatch "Invalid format for mcu description:\n" invalid
