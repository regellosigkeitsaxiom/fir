module Commander where

import Types
import Info
import MCU
import Config
import Prelude hiding ( FilePath )
import System.Environment ( getExecutablePath )
import Filesystem.Path.CurrentOS
import Filesystem.Path
import System.Process
import System.Directory ( createDirectoryIfMissing, doesFileExist )
import Control.Monad
import Rainbow
import System.IO hiding ( FilePath )
import Text.Read
import Data.List
import Data.Yaml ( encodeFile )
import Data.Maybe
import Safe

blandFlags :: [ String ]
blandFlags =
  [ "-mthumb"
  , "--specs=nosys.specs"
  , "-Wl,--gc-sections"
  --, "-fstack-usage"
  ]

coreFlag :: MCU -> String
coreFlag mcu = "-mcpu=" ++ f ( core mcu )
  where
  f CortexM0 = "cortex-m0"
  f CortexM0plus = "cortex-m0plus"
  f CortexM3 = "cortex-m3"
  f CortexM4 = "cortex-m4"
  f CortexM7 = "cortex-m7"

getDatabase :: IO FilePath
getDatabase = do
  x <- directory . decodeString <$> getExecutablePath
  return $ collapse $ x </> decodeString "../database"

includeFlag :: IO [ String ]
includeFlag = do
  database <- getDatabase
  let incs = map ( append database ) dirs
  return $ map (\s-> "-I" ++ encodeString s ) incs
  where
  dirs = map decodeString
    [ "CMSIS"
    , "HAL"
    ]

patchDB :: String -> IO String
patchDB f = do
  db <- getDatabase
  return $ encodeString $ append db $ decodeString f

allFlags :: MCU -> DotFir -> String -> IO [ String ]
allFlags mcu info file = do
  incs <- includeFlag
  database <- getDatabase
  return $ incs ++ flags database
  where
  addDB db f = encodeString $ append db $ decodeString f
  flags db =
    [ coreFlag mcu ] ++
    blandFlags ++
    [ "-D" ++ define mcu ] ++
    [ "-T" ++ ( addDB db $ "linkers/" ++ linker info ++ ".ld" )] ++
    [ addDB db $ "startups/" ++ startup mcu ] ++
    [ "-o./build/" ++ getBaseName file ++ ".elf" ] ++
    [ file ]

getBaseName = encodeString . basename . decodeString

buildELF :: MCU -> DotFir -> String -> IO ()
buildELF mcu info file = do
  createDirectoryIfMissing False "build"
  callProcess "arm-none-eabi-gcc" =<< allFlags mcu info file

buildBIN :: String -> IO ()
buildBIN file = do
  callProcess "arm-none-eabi-objcopy"
    [ "-Obinary"
    , "./build/" ++ getBaseName file ++ ".elf"
    , "./build/" ++ getBaseName file ++ ".bin"
    ]

findMCU :: DotFir -> [ MCU ] -> Maybe MCU
findMCU fir mcus = find (\x -> fullname x == model fir ) mcus

builder :: String -> IO ()
builder file = do
  info <- either undefined id <$> getInfo --FIXME
  dbmcu <- patchDB "mcu"
  mcu <- fromMaybe undefined . findMCU info <$> readAllMCU dbmcu --FIXME
  buildELF mcu info file
  buildBIN file

setter :: IO ()
setter = do
  que <- doesFileExist ".fir.yaml"
  dbmcu <- patchDB "mcu"
  mcus <- readAllMCU dbmcu
  putStrLn "Select target MCU (only shown are available):"
  target <- pickBy fullname mcus
  case length $ linkers target of
    0 -> error "No linkers in MCU specification, aborting"
    1 -> encodeFile ".fir.yaml" $ DotFir ( fullname target ) ( head $ linkers target )
    _ -> do
      putStrLn "Multiple linkers are available. Please select one:"
      linker <- pickBy id $ linkers target
      encodeFile ".fir.yaml" $ DotFir ( fullname target ) linker
  
pickBy :: ( a -> String ) -> [ a ] -> IO a
pickBy f variants = do
  mapM_ printOne $ zip [1..] $ map f variants
  hFlush stdout
  choice <- getLine
  return $ case readMaybe choice :: Maybe Int of
    Just n -> if length variants >= n then variants !! (n-1) else error "Too big number, aborting"
    Nothing -> error "Invalid input, aborting"
  where
  printOne :: ( Int, String ) -> IO ()
  printOne (ix,var) = do
    putChunk $ bold $ chunk ( show ix ) & back black & fore red
    putChunkLn $ chunk ( take ( 3 - length ( show ix )) ( repeat ' ' ) ++ var ) & back black & fore white

flashWrapper :: Maybe String -> Maybe String -> IO ()
flashWrapper point file = do
  info <- getFirConfig
  flasher info point file

flasher :: FirConfig -> Maybe String -> Maybe String -> IO ()
flasher fc pointName binary = do
  let que = (\p -> find (\x -> name x == p ) $ flashPoints fc ) =<< pointName
  case que of
    Nothing -> error "Pick flashpoint from available"
    Just fp -> flash fp ( fromMaybe "main.bin" binary )
  where
  flash :: FlashPoint -> String -> IO ()
  flash fp file = callProcess ( fromMaybe "st-flash" $ command fp )
    [ "--reset", "write", "build/" ++ file, "0x8000000" ]
