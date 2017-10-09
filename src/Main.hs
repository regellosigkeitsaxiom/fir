module Main where

import Info
import Commander
import Help
import System.Environment ( getArgs )
import Safe

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["build"] -> builder "main.c"
    ["build",file] -> builder file
    ["init"] -> setter
    --["flash"] -> undefined
    --["all"] -> undefined
    ["help"] -> helpDoc
    [] -> do
      putStrLn "config > remotes | options | init"
      putStrLn "help"
      putStrLn "build"
      putStrLn "flash"
      putStrLn "all"
      putStrLn "init (should ask for templates)"
    ("flash":rest) ->
      flashWrapper ( atMay rest 0 ) ( atMay rest 1 )
    _ -> putStrLn "Not implemented yet"
