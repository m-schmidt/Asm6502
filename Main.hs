module Main

where

import AsmOutput
import AsmParser
import Assembler
import Commandline
import Control.Monad
import Error
import System.Environment
import System.FilePath
import System.IO.Error
import qualified Data.ByteString as B (readFile, unpack)
import qualified Data.Set as Set


main :: IO ()
main = do

  -- Parse command line and input program
  (opts,filename) <- getArgs >>= resolveAtFiles >>= commandLineOptions
  p <- catchIO parseAssembler filename

  -- Check for common errors
  let (duplictes,table) = symbolTable p
  when (not $ Set.null duplictes) $ exitWithError $ formattedList "Error: multiply defined symbols:\n" $ Set.toList duplictes

  let undefs = undefinedSymbols table p
  when (not $ Set.null undefs) $ exitWithError $ formattedList "Error: undefined symbols:\n" $ Set.toList undefs

  let unused = unusedSymbols table p
  when (not $ Set.null unused) $ report $ formattedList "Warning: unused symbols:\n" $ Set.toList unused

  let illegals = illegalInstructions p
  when (illegals /= []) $ exitWithError $ formattedList "Error: illegal instructions/operand combinations:\n" illegals

  -- Strip unused program parts and split it into chunks
  let p_sort = sortedChunks $ stripUnused unused p

  -- Write minimized input for debugging
  when (optDumpParsed opts) $ catchIO (writeParsed p_sort) $ prependExtension filename "parsed"

  -- Load included files
  p_include <- resolveIncludes (takeDirectory filename) p_sort

  -- Compute final label addresses and resolve all symbolic references in the program
  let table_resolved = resolveLabels table p_include
  let p_resolved = resolveReferences table_resolved p_include

  -- Check for relative branches that have targets out of range
  let branches = overlongBranches p_resolved
  when (branches /= []) $ exitWithError $ formattedList "Error: overlong relative branches:\n" branches

  -- Assemble chunks
  let p_assembled = assemble p_resolved

  -- Check for overlapping program parts
  let overlaps = overlappingParts p_assembled
  when (overlaps /= []) $ exitWithError $ formattedList "Error: ORG directives conflict with existing code or data:\n" overlaps

  -- Merge the assembled program into a single stream of bytes
  let p_final@(AsmChunk start bytes) = merge p_assembled

  -- Check that program remains within 64k address space
  let excess = start + length bytes - 65536
  when (excess > 0) $ exitWithError $ "Error: program exceeds available address space by " ++ show excess ++ " bytes.\n"

  -- Write symboltable for debugging
  when (optShowSymtab opts) $ report $ formattedList "Resolved symbols:\n" $ describeSymbolTable table_resolved

  -- Write output file
  let writer = case (optFormat opts) of
                 HEX -> writeHEX
                 PRG -> writePRG

  catchIO (writer p_final) $ optOutput opts


-- Handle IO exceptions when reading input files
catchIO :: (FilePath -> IO a) -> FilePath -> IO a
catchIO action path = catchIOError (action path) handler
  where
    handler e
      | isDoesNotExistError e = exitWithError $ "Error: reading file '" ++ path ++ "' failed since it doesn't exist."
      | isPermissionError e   = exitWithError $ "Error: permission to access file '" ++ path ++ "' denied."
      | otherwise             = exitWithError $ "Error: unknown error while accessing file '" ++ path ++ "'."


-- Incorporate contents of '@file' linewise into the command line arguments
resolveAtFiles :: [String] -> IO [String]
resolveAtFiles = fmap (filter (/= []) . concat) . mapM resolveAtFile
  where
    resolveAtFile ('@':path) = lines <$> catchIO readFile path
    resolveAtFile s = return [s]


-- Replace include statements with data contents of referenced files
resolveIncludes :: FilePath -> [Chunk] -> IO [Chunk]
resolveIncludes p = mapM (\(Chunk a ss) -> mapM resolve ss >>= return . (Chunk a))
  where
    resolve st = case st of
      Include f -> catchIO B.readFile (combine p f) >>= return . Data 1 . map (Constant . fromIntegral) . drop 2 . B.unpack
      _         -> return st


-- Add a further extension in front of the already existing extensions of a filepath
prependExtension :: FilePath -> String -> FilePath
prependExtension path ext = (dropExtensions path) <.> ext <.> (takeExtensions path)
