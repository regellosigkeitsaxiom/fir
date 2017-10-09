{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Config where

import Data.Aeson.TH
import Data.Yaml
import System.IO
import System.Directory

data SSHEntry = SSHEntry
  { address
  , key
  , user
  , port :: String
  } deriving ( Show )

data FlashPoint = FlashPoint
  { name
  , path :: String
  , command :: Maybe String
  , ssh :: Maybe SSHEntry
  } deriving ( Show )

data FirConfig = FirConfig
  { pdfReader
  , cmsisReader :: String 
  , flashPoints :: [ FlashPoint ]
  } deriving ( Show )

$(deriveJSON defaultOptions ''SSHEntry)
$(deriveJSON defaultOptions ''FlashPoint)
$(deriveJSON defaultOptions ''FirConfig)

getFirConfig :: IO FirConfig
getFirConfig = do
  home <- getHomeDirectory
  z <- withCurrentDirectory home $ doesFileExist ".firrc.yaml"
  case z of
    True -> do
      try <- withCurrentDirectory home $ decodeFile ".firrc.yaml"
      case try of
        Just x -> return x
        Nothing -> error "Could not read ~/.firrc.yaml"
    False -> do
      que <- ask "Config ~/.firrc.yaml not found. I can create template with you. Go? "     
      case que of
        "y" -> do
          newFC <- initConfig
          home <- getHomeDirectory
          writeConfig newFC
          return newFC
          

writeConfig :: FirConfig -> IO ()
writeConfig fc = do
  home <- getHomeDirectory
  withCurrentDirectory home $ encodeFile ".firrc.yaml" fc

initConfig :: IO FirConfig --Make it use haskeline
initConfig = do
  putStrLn "I will generate basic config file for you and place it to ~/.firrc.yaml"
  pdfR <- ask "What is your preferred PDF reader? "
  cmsisR <- ask "What is your preferred text editor for browsing CMSIS and stuff? "
  putStrLn "I will create two default flashpoints: `local` for STLink V1 and `here` for STLink V2"
  yn <- ask "Do you need to create any other flashpoints (possibly ssh)? y/_ "
  newFPs <- case yn of
    "y" -> do
      newName <- ask "Name your flashpoint: "
      ver <- ask "What is version of STLink? 1 or 2? "
      newVer <- case ver of
        "1" -> return "/dev/disk/by-id/usb-STM32_STM32_STLink-0:0"
        "2" -> return "/dev/stlinkv2_1"
        _ -> undefined
      yn2 <- ask "Will you use ssh?"
      newSSH <- case yn2 of
        "y" -> do
          newAddr <- ask "Enter address: "
          newKey <- ask "Enter key location: "
          newPort <- ask "Enter port: "
          newUser <- ask "Enter user: "
          return $ Just $ SSHEntry newAddr newKey newUser newPort
        "n" -> return Nothing
        _ -> undefined
      yn3 <- ask "Will it use custom flash command? "
      newComm <- case yn3 of
        "n" -> return Nothing
        "y" -> Just <$> ask "Please enter it: "
        _ -> undefined
      return [ FlashPoint newName newVer newComm newSSH ]
    "n" -> return []
    _ -> undefined
  return $ FirConfig pdfR cmsisR $ here:local:newFPs
  where
  here = FlashPoint "here" "/dev/stlinkv2_1" Nothing Nothing
  local = FlashPoint "here" "/dev/disk/by-id/usb-STM32_STM32_STLink-0:0" Nothing Nothing

ask :: String -> IO String
ask s = do
  putStr s
  hFlush stdout
  getLine
