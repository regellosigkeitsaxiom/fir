module Commander where

import Types
import Info
import MCU
import Prelude hiding ( FilePath )
import System.Environment ( getExecutablePath )
import Filesystem.Path.CurrentOS
import Filesystem.Path
import System.Process
import System.Directory ( createDirectoryIfMissing )

blandFlags :: [ String ]
blandFlags =
  [ "-mthumb"
  , "--specs=nosys.specs"
  , "-Wl,--gc-sections"
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
    , "mcu"
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
    [ "-o./build/main.elf" ] ++ --not main!
    [ file ]

buildELF :: MCU -> DotFir -> String -> IO ()
buildELF mcu info file = do
  createDirectoryIfMissing False "build"
  callProcess "arm-none-eabi-gcc" =<< allFlags mcu info file

buildBIN :: String -> IO ()
buildBIN file = do
  let bn = encodeString . basename . decodeString $ file
  callProcess "arm-none-eabi-objcopy"
    [ "-Obinary"
    , "./build/" ++ bn ++ ".elf"
    , "./build/" ++ bn ++ ".bin"
    ]

builder :: String -> IO ()
builder file = do
  info <- ( either undefined id ) <$> getInfo
  dbmcu <- patchDB "mcu"
  mcu <- head <$> readAllMCU dbmcu
  buildELF mcu info file
  buildBIN file
