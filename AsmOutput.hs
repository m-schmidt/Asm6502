module AsmOutput
  ( writePRG
  , writeHEX
  , writeParsed
  )

where

import Arch6502
import AsmParser
import Assembler
import qualified Data.ByteString as B (writeFile, pack)
import Text.Printf


-- Write assembled program in PRG output format
writePRG :: AsmChunk -> FilePath -> IO ()
writePRG (AsmChunk address bytes) filename =
  B.writeFile filename $ B.pack $ (encodeOperand 2 address) ++ bytes


-- Write assembled program in HEX output format
writeHEX :: AsmChunk -> FilePath -> IO ()
writeHEX (AsmChunk address bytes) filename =
  writeFile filename $ unlines $ hexDump address bytes
  where
    hexDump _ [] = []
    hexDump a bs = (hexLine a $ take 16 bs) : hexDump (a+16) (drop 16 bs)
    hexLine a bs = (printf "%04x:" a) ++ (concat $ map (printf " %02x") bs)


-- Write parsed assembler source program to a file
writeParsed :: [Chunk] -> FilePath -> IO ()
writeParsed chunks filename = writeFile filename . unlines . map writeChunk $ chunks
  where
    writeChunk (Chunk address stmts) =
      unlines $ ("\torg\t" ++ writeHexConst address) : map writeStatement stmts

    writeStatement st = case st of
      Data 1 (v:[])   -> "\tbyte\t" ++ (writeValue v)
      Data 2 (v:[])   -> "\tword\t" ++ (writeValue v)
      Data _ vs       -> "\thexdata\t" ++ (unwords $ map writeValue vs)
      Equ k v         -> "\t" ++ k ++ "\tequ\t" ++ (writeValue v)
      Include p       -> "\tinclude-prg\t\"" ++ p ++ "\""
      Instruction m o -> "\t" ++ (show m) ++ (writeOperand o)
      Label l         -> l ++ ":"
      Org v           -> "\torg\t" ++ (writeValue v)

    writeOperand o = case o of
      Operand Absolute v        -> "\t" ++ (writeValue v)
      Operand AbsoluteX v       -> "\t" ++ (writeValue v) ++ ",X"
      Operand AbsoluteY v       -> "\t" ++ (writeValue v) ++ ",Y"
      Operand Accumulator _     -> "\tA"
      Operand Immediate v       -> "\t#" ++ (writeValue v)
      Operand IndexedIndirect v -> "\t(" ++ (writeValue v) ++ ",X)"
      Operand Indirect v        -> "\t(" ++ (writeValue v) ++ ")"
      Operand IndirectIndexed v -> "\t(" ++ (writeValue v) ++ "),Y"
      Operand Implied _         -> ""
      Operand Relative v        -> "\t" ++ (writeValue v)
      Operand ZeroPage v        -> "\t" ++ (writeValue v)
      Operand ZeroPageX v       -> "\t" ++ (writeValue v) ++ ",X"
      Operand ZeroPageY v       -> "\t" ++ (writeValue v) ++ ",Y"

    writeValue v = case v of
      Constant c -> writeHexConst c
      Symbol s 0 -> s
      Symbol s o -> s ++ (printf "%+d" o)

    writeHexConst n =
      if n < 256
        then printf "$%02x" n
        else printf "$%04x" n
