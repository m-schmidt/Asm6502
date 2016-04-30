module Error
  ( report
  , exitWithInfo
  , exitWithError
  , formattedList
  )

where

import Data.List
import System.Exit
import System.IO


-- Report message to stderr
report :: String -> IO ()
report message = do
  hPutStrLn stderr message


-- Report info/error messages and terminate the programm
exitWithInfo :: String -> IO a
exitWithInfo message = do
  report message
  exitWith ExitSuccess

exitWithError :: String -> IO a
exitWithError message = do
  report message
  exitWith $ ExitFailure 1


-- Convert a list of strings into a readable form for error messages
formattedList :: String -> [String] -> String
formattedList prefix = (prefix++) . unlines . map (" - "++) . sort
