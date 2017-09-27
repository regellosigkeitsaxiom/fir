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

data HelpMe
  = HelpLibrary FilePath
  | HelpDocument FilePath
  deriving ( Show )

listAvailableHelp :: MCU -> IO HelpMe
listAvailableHelp mcu = do
  putStrLn "Avaliable reference manuals: "
  let x1 = length $ reference $ manuals mcu
  let x2 = length $ datasheet $ manuals mcu
  let x3 = length $ errata    $ manuals mcu
  let x4 = length $ other     $ manuals mcu
  makeSelector 1 $ reference $ manuals mcu
  makeSelector (1+x1) $ datasheet $ manuals mcu
  makeSelector (1+x1+x2) $ errata $ manuals mcu
  makeSelector (1+x1+x2+x3) $ other $ manuals mcu
  i <- getLine
  case readMaybe i :: Maybe Int of
    Just j ->
      if | j > x1+x2+x3+x4 || j <= 0 -> undefined
         | otherwise -> return $ HelpDocument . file $ ( (\f-> f $ manuals mcu ) =<< [ reference, datasheet, errata, other ] ) !! j
    Nothing -> undefined

makeSelector :: Int -> [ Document ] -> IO ()
makeSelector ix li = mapM_ makeOpt $ zip [ix..] li
  where
  makeOpt :: ( Int, Document ) -> IO ()
  makeOpt (i,a) = putStrLn $ ( show i ) ++
    ( take ( 3 - ( length $ show i )) "   " ) ++
    ( fromMaybe ( file a ) ( code a ) ++ " | " ++ description a )

helpDoc = do
  info <- either undefined id <$> getInfo --FIXME
  dbmcu <- patchDB "mcu"
  mcu <- fromMaybe undefined . findMCU info <$> readAllMCU dbmcu --FIXME
  help <- listAvailableHelp mcu
  callHelp help

callHelp :: HelpMe -> IO ()
callHelp ( HelpLibrary f ) = undefined
callHelp ( HelpDocument f ) = undefined
