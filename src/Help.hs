{-# LANGUAGE MultiWayIf #-}
module Help where

import System.IO
import Types
import Config
import MCU
import Info
import Data.Maybe
import Commander
import Text.Read
import System.Process
import Data.Yaml
import System.Directory

data HelpMe
  = HelpLibrary FilePath
  | HelpStartup FilePath
  | HelpLinker FilePath
  | HelpDocument FilePath
  deriving ( Show )

listAvailableHelp :: MCU -> IO HelpMe
listAvailableHelp mcu = do
  putStrLn "Related documents: "
  let x1 = length $ reference $ manuals mcu
  let x2 = length $ datasheet $ manuals mcu
  let x3 = length $ errata    $ manuals mcu
  let x4 = length $ other     $ manuals mcu
  let all = 1 + x1 + x2 + x3 + x4
  makeSelectorDoc 1 $ reference $ manuals mcu
  makeSelectorDoc (1+x1) $ datasheet $ manuals mcu
  makeSelectorDoc (1+x1+x2) $ errata $ manuals mcu
  makeSelectorDoc (1+x1+x2+x3) $ other $ manuals mcu
  putStrLn "\nRelated libraries and stuff: "
  makeSelectorLib (all) "NOT YET" "CMSIS library"
  makeSelectorLib (all+1) ( show $ core mcu ) "Core library"
  makeSelectorLib (all+2) ( startup mcu ) "Startup file"
  --makeSelector (all+4) $ "Linker file: " ++ startup mcu
  --makeSelector (all+4) $ "Other libraries: " ++ startup mcu
  i <- getLine
  case readMaybe i :: Maybe Int of
    Just j ->
      if | j >= all+(length $ cmsis mcu)+2 || j <= 0 -> error "Invalid selection"
         | j >= all && j < all+(length $ cmsis mcu) -> return $ HelpLibrary $ error "CMSIS pick"
         | j == all+(length $ cmsis mcu) -> return $ HelpLibrary $ error "Core pick"
         | j == all+(length $ cmsis mcu)+1 -> return $ HelpStartup $ startup mcu
         | otherwise -> return $ HelpDocument . file $ ( (\f-> f $ manuals mcu ) =<< [ reference, datasheet, errata, other ] ) !! (j-1)
    Nothing -> error "Invalid input"

mkOpt :: Int -> String -> String -> IO ()
mkOpt ix name descr = putStrLn $ show ix
  ++ ( pad 3 ix )
  ++ name
  ++ pad 23 name
  ++ " | "
  ++ descr
  where
  pad n x = take ( n - ( length $ show x )) $ repeat ' '

makeSelectorLib = mkOpt

makeSelectorDoc :: Int -> [ Document ] -> IO ()
makeSelectorDoc ix li = mapM_ makeOpt $ zip [ix..] li
  where
  makeOpt :: ( Int, Document ) -> IO ()
  makeOpt (i,a) = mkOpt i ( fromMaybe (file a) (code a)) (description a)

helpDoc = do
  info <- either (error ".fir.yaml") id <$> getInfo --FIXME
  dbmcu <- patchDB "mcu"
  mcu <- fromMaybe (error "mcu") . findMCU info <$> readAllMCU dbmcu --FIXME
  help <- listAvailableHelp mcu
  home <- getHomeDirectory
  cfg <- withCurrentDirectory home $ decodeFileEither ".firrc.yaml"
  case cfg of
    Left err -> error $ show err
    Right c -> callHelp help c

callHelp :: HelpMe -> FirConfig -> IO ()
callHelp ( HelpLinker f ) cfg = do
  dbdoc <- patchDB "linkers"
  let file = dbdoc ++ "/" ++ f
  callProcess ( cmsisReader cfg ) [ file ]
callHelp ( HelpStartup f ) cfg = do
  dbdoc <- patchDB "startups"
  let file = dbdoc ++ "/" ++ f
  callProcess ( cmsisReader cfg ) [ file ]
callHelp ( HelpLibrary f ) cfg = do
  dbdoc <- patchDB "CMSIS"
  let file = dbdoc ++ "/" ++ f
  callProcess ( cmsisReader cfg ) [ file ]
callHelp ( HelpDocument f ) cfg = do
  dbdoc <- patchDB "documents"
  let file = dbdoc ++ "/" ++ f
  callProcess ( pdfReader cfg ) [ file ]
