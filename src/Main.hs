module Main where

import Info
import Commander
import System.Environment ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["build"] -> builder "main.c"
    ["build",file] -> builder file
    ["init"] -> setter
    _ -> putStrLn "Not implemented yet"
