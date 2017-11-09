module Main where

import Info
import Commander
import Help
import System.Environment ( getArgs )
import Safe
import Config
import Data.Maybe ( fromMaybe )

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
    ("all":rest) ->
      allWrapper ( atMay rest 0 )
    ("flash":rest) ->
      flashWrapper ( atMay rest 0 ) ( atMay rest 1 )
    ["remotes"] -> do
      conf <- getFirConfig
      mapM_ printFlashPoint $ flashPoints conf
    _ -> do
      putStrLn "help (documentation and libraries)"
      putStrLn "build [<file>]"
      putStrLn "flash [<remote> [<file>]]"
      putStrLn "all [<remote>]"
      putStrLn "init (will ask for templates)"
      putStrLn "remotes"
      putStrLn "anything else prints this message"

printFlashPoint :: FlashPoint -> IO ()
printFlashPoint fp = do
  putStrLn $ "Name    | " ++ name fp
  putStrLn $ "Path    | " ++ path fp
  putStrLn $ "Command | " ++ fromMaybe "st-flash" ( command fp )
  case ssh fp of
    Nothing -> putStrLn "SSH     | NONE"
    Just s -> do
      putStrLn   "SSH:"
      putStrLn $ "Address | " ++ address s
      putStrLn $ "User    | " ++ user s
      putStrLn $ "Port    | " ++ port s
      putStrLn $ "Key     | " ++ key s
  putStrLn ""
