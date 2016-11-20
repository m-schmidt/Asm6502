-- |Module to parse the command line
module Commandline
  ( OutFormat(..)
  , Options(..)
  , commandLineOptions
  )

where

import Control.Monad
import Data.Char
import Data.List
import Error
import System.Console.GetOpt
import System.FilePath
import Text.Read


-- |Supported output formats
data OutFormat
  = PRG             -- ^ A binary format directly loadable by an emulator
  | HEX             -- ^ A textual hexdump
  deriving (Eq,Show,Read)


-- |Command line options of the assembler
data Options = Options
  { optDumpParsed  :: Bool        -- ^ Tracing: dump parsed input program
  , optFormat      :: OutFormat   -- ^ Parsed output format
  , optFormatRaw   :: String      -- ^ Raw output format as specified on command line
  , optOutput      :: FilePath    -- ^ The output file
  , optShowHelp    :: Bool        -- ^ Show help and terminate program
  , optShowSymtab  :: Bool        -- ^ Tracing: print all symbols with their resolved addresses
  , optShowVersion :: Bool        -- ^ Show version information and terminate program
  } deriving (Eq,Show)


-- |Default values for command line options
defaultOptions :: Options
defaultOptions     = Options
  { optDumpParsed  = False
  , optFormat      = PRG
  , optFormatRaw   = ""
  , optOutput      = ""
  , optShowHelp    = False
  , optShowSymtab  = False
  , optShowVersion = False
  }


-- |Option descriptions for GetOpt module
options :: [OptDescr (Options -> Options)]
options =
  [ Option ['h','?']
      ["help"]
      (NoArg (\opts -> opts { optShowHelp = True }))
      "Print this help message."
  , Option ['v']
      ["version"]
      (NoArg (\opts -> opts { optShowVersion = True }))
      "Print the version information."
  , Option ['s']
      ["symtab"]
      (NoArg (\opts -> opts { optShowSymtab = True }))
      "Print the symbol table."
  , Option ['d']
      ["dparse"]
      (NoArg (\opts -> opts { optDumpParsed = True }))
      "Print the parsed input program."
  , Option ['o']
      ["output"]
      (ReqArg (\s opts -> opts { optOutput = s }) "FILE")
      ("Set output file. Defaults to '<input>.prg' or '<input>.hex'")
  , Option ['f']
      ["format"]
      (ReqArg (\s opts -> opts { optFormatRaw = s }) "FORMAT")
      ("Set output format to 'PRG' or 'HEX'. Defaults to '" ++ show (optFormat defaultOptions) ++ "'.")
  ]


-- |Command line handling
commandLineOptions :: [String] -> IO (Options, String)
commandLineOptions argv =
  case getOpt Permute options argv of
    (o,n,[]) -> do
      when (optShowHelp o') showHelp
      when (optShowVersion o') showVersion
      file <- handleInputFile n
      opts <- handleOutputFormat o' >>= handleOutputFile file
      return (opts, file)

      where
        o' = foldl (flip id) defaultOptions o

    (_,_,errs) -> exitWithError $ concat (map ("Error: "++) $ nub errs) ++ usageInfo header options


-- |Header message for usage info
header :: String
header = "Synopsis: asm6502 [options] <input file>"


-- |Show help and exit programm
showHelp :: IO ()
showHelp = exitWithInfo (usageInfo header options)


-- |Show version info and exit programm
showVersion :: IO ()
showVersion = exitWithInfo "Asm6502 version 1"


-- |Check for a single input file
handleInputFile :: [String] -> IO String
handleInputFile files = do
  case files of
    f:[] -> return $ makeValid f
    _    -> exitWithError "Error: please specify exactly one input file"


-- |Check for a valid output format specification
handleOutputFormat :: Options -> IO Options
handleOutputFormat opts
  | null format = return opts
  | otherwise   = case readMaybe format of
                    Just f  -> return opts { optFormat = f }
                    Nothing -> exitWithError $ "Error: illegal output format '" ++ format ++ "'"
  where
    format = map toUpper $ optFormatRaw opts


-- |Check for name of output file, generate one from input file if missing
handleOutputFile :: FilePath -> Options -> IO Options
handleOutputFile file opts
  | null output = return opts { optOutput = replaceExtension file $ map toLower $ show $ optFormat opts }
  | otherwise   = return opts
  where
    output = optOutput opts
