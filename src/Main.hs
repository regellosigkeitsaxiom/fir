module Main where

import Info
import Commander
import System.Environment ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  print args
  case args of
    ["info"] -> showCurrentInfo
    [ file ] -> builder file
    _ -> putStrLn "Not implemented yet"
