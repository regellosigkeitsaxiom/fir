module MCU where

import Types

import qualified Data.ByteString as BS
import Data.Yaml 
import System.IO ( hPutStrLn, stderr )
import System.Directory
import Control.Monad
import Data.Maybe ( catMaybes )

readOneMCU :: FilePath -> IO ( Maybe MCU )
readOneMCU file = do
  x <- decodeEither <$> BS.readFile file
  case x of
    Left e -> do
      hPutStrLn stderr "Error in function readOneMCU"
      hPutStrLn stderr e
      return Nothing
    Right m -> return m

readAllMCU :: FilePath -> IO [ MCU ]
readAllMCU dir = do
  path <- canonicalizePath dir
  files <- listDirectory dir
  let fullfiles = map (\x-> path ++ "/" ++ x ) files
  catMaybes <$> mapM readOneMCU fullfiles
