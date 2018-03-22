module Commander where

import Types
import Info
import MCU
import Config
import Prelude hiding ( FilePath )
import System.Environment ( getExecutablePath )
import Filesystem.Path.CurrentOS hiding ( null )
import Filesystem.Path hiding ( null )
import System.Process
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , listDirectory
  , copyFile
  , readable
  , writable
  , setPermissions
  , getPermissions
  )
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
    --, "HAL"
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
    options info ++
    [ "-D" ++ define mcu ] ++
    [ "-T" ++ addDB db ( "linkers/" ++ linker info ++ ".ld" )] ++
    [ addDB db $ "startups/" ++ startup mcu ] ++
    [ "-o./build/" ++ getBaseName file ++ ".elf" ] ++
    [ "-g" ] ++
    [ file ]

getBaseName = encodeString . basename . decodeString

buildELF :: MCU -> DotFir -> String -> IO ()
buildELF mcu info file = do
  createDirectoryIfMissing False "build"
  callProcess "arm-none-eabi-gcc" =<< allFlags mcu info file

buildBIN :: String -> IO ()
buildBIN file =
  callProcess "arm-none-eabi-objcopy"
    [ "-Obinary"
    , "./build/" ++ getBaseName file ++ ".elf"
    , "./build/" ++ getBaseName file ++ ".bin"
    ]

callSplint :: [ String ] --File name and/or other options
           -> IO ()
callSplint comms = do
  includes <- includeFlag
  callProcess "splint" $ includes ++ comms

findMCU :: DotFir -> [ MCU ] -> Maybe MCU
findMCU fir = find (\x -> fullname x == model fir )

builder :: String -> IO ()
builder file = do
  info <- either error id <$> getInfo --FIXME
  dbmcu <- patchDB "mcu"
  mcu <- fromMaybe (error "findMCU failed") . findMCU info <$> readAllMCU dbmcu --FIXME
  buildELF mcu info file
  buildBIN file

setter :: IO ()
setter = do
  que <- doesFileExist ".fir.yaml"
  if que
  then error "You already have .fir.yaml, I don't know how to proceed" --Should propose template
  else do
    dbmcu <- patchDB "mcu"
    mcus <- readAllMCU dbmcu
    putStrLn "Select target MCU (only shown are available):"
    target <- pickBy fullname mcus
    case length $ linkers target of
      0 -> error "No linkers in MCU specification, aborting"
      1 -> encodeFile ".fir.yaml" $ DotFir ( fullname target ) ( head $ linkers target ) ["-O2", "-Werror"]
      _ -> do
        putStrLn "Multiple linkers are available. Please select one:"
        linker <- pickBy id $ linkers target
        encodeFile ".fir.yaml" $ DotFir ( fullname target ) linker ["-O2","-Werror"]
    if not . null $ templates target
    then do
      dir <- listDirectory "."
      if length dir > 1
      then putStrLn "I see you have some stuff here, so I won't bug you with templates"
      else do
        putStrLn "\nI see you have empty directory here. A have some templates available:"
        templ <- pickBy descr $ templates target
        dbTemplates <- patchDB $ "examples/" ++ location templ
        templateFiles <- filter (/= ".fir.yaml") <$> listDirectory dbTemplates
        mapM_ ( copyWritable dbTemplates ) templateFiles
        putStrLn "Example copied to current directory"
    else putStrLn "I don't have templates for this MCU, so I'm done here"
    where
    copyWritable templ x = do
      copyFile ( templ ++ "/" ++ x ) x
      p <- getPermissions x
      setPermissions x $ p { readable = True, writable = True }
  
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
    putChunkLn $ chunk ( replicate ( 3 - length ( show ix )) ' ' ++ var ) & back black & fore white

allWrapper :: Maybe String -> IO ()
allWrapper point = do
  info <- getFirConfig
  builder "main.c"
  flasher info point Nothing

resetWrapper :: Maybe String -> IO ()
resetWrapper point = do
  info <- getFirConfig
  resetter info point

resetter :: FirConfig -> Maybe String -> IO ()
resetter fc pointName = do
  let que = (\p -> find (\x -> name x == p ) $ flashPoints fc ) =<< pointName
  fp <- case que of
    Nothing -> getFP $ flashPoints fc
    Just fp -> return fp
  case ssh fp of
    Nothing -> callProcess ( fromMaybe "st-flash" $ command fp ) [ "reset" ]
    Just sshe -> callProcess "ssh"
      [ "-p" ++ port sshe
      , "-i" ++ key sshe
      , user sshe ++ "@" ++ address sshe
      , fromMaybe "st-flash" ( command fp ) ++ " reset"
      ]

flashWrapper :: Maybe String -> Maybe String -> IO ()
flashWrapper point file = do
  info <- getFirConfig
  flasher info point file

flasher :: FirConfig -> Maybe String -> Maybe String -> IO ()
flasher fc pointName binary = do
  let que = (\p -> find (\x -> name x == p ) $ flashPoints fc ) =<< pointName
  fp <- case que of
    Nothing -> getFP $ flashPoints fc
    Just fp -> return fp
  case ssh fp of
    Nothing -> flashLocal fp "build/main.bin"
    Just sshe -> flashRemote sshe ( command fp ) $ fromMaybe "build/main.bin" binary

flashLocal :: FlashPoint -> String -> IO ()
flashLocal fp file = callProcess ( fromMaybe "st-flash" $ command fp )
  [ "write", {-"build/" ++ -} file, "0x8000000" ]

getFP :: [ FlashPoint ] -> IO FlashPoint
getFP fps = do
  putStrLn "You did not specify flash point. Please pick one:"
  mapM_ printFP $ zip [1..] fps
  hFlush stdout
  i <- getLine
  case readMaybe i :: Maybe Int of
    Nothing -> error "I honestly expected integer. Aborting"
    Just j ->
      if j < 1 || j > length fps
      then error "Number out of range"
      else return $ fps !! (j-1)
  where
  printFP :: ( Int, FlashPoint ) -> IO ()
  printFP (ix,fp) = do
    putStr $ show ix ++ "  "
    putStr $ if isNothing ( ssh fp ) then "" else "[ssh] "
    putStr $ name fp
    putStrLn $ fromMaybe "" $ (" // " ++) <$> comment fp

flashRemote :: SSHEntry -> Maybe String -> String -> IO ()
flashRemote e flashCom file = do
  putStrLn $ "scp -P" ++ port e ++ " -i" ++ key e ++ " " {- ++ "build/" -} ++ file ++ " " ++ user e ++"@" ++ address e ++ ":" ++ "firmware.bin"
  callProcess "scp"
    [ "-P" ++ port e
    , "-i" ++ key e
    , {-"build/" ++ -} file
    , user e ++ "@" ++ address e ++ ":" ++ "firmware.bin"
    ]
  putStrLn $ "ssh -p" ++ port e ++ " -i" ++ key e ++ " " ++ user e ++"@" ++ address e ++ " " ++ fromMaybe "st-flash" flashCom ++ " write firmware.bin 0x8000000"
  callProcess "ssh"
    [ "-p" ++ port e
    , "-i" ++ key e
    , user e ++ "@" ++ address e
    , fromMaybe "st-flash" flashCom ++ " write firmware.bin 0x8000000" 
    ]
