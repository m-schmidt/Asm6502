-- |Assembling instructions into bute sequences
module Assembler where

import AsmParser
import Arch6502
import Data.List
import Data.Map (Map)
import Data.Word
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Printf


-- |A symboltable maps symbols to their eventually resolved addresses
type SymbolTable = Map String (Maybe Int)

-- |Sets of duplicate symbols
type Duplicates  = Set String


-- |Computes a tuple containing all duplicate symbols and a table mapping symbols to their addresses
symbolTable :: [Statement] -> (Duplicates, SymbolTable)
symbolTable = foldr accumulate (Set.empty, Map.empty) . concat . map definitions
  where
    -- accumulate symbol definitions
    accumulate (k, v) (dups, table) =
      if Map.member k table
        then (Set.insert k dups, table)
        else (dups, Map.insert k v table)

    -- extract symbol definitions from the program statements
    definitions st = case st of
      Equ k (Constant v) -> [(k,Just v)]
      Label l            -> [(l,Nothing)]
      _                  -> []


-- |Convert a symbol table into a list of strings describing all resolved symbols
describeSymbolTable :: SymbolTable -> [String]
describeSymbolTable = Map.foldrWithKey describeSymbol []
  where
    describeSymbol sym (Just addr) acc = (printf "$%04x = " addr ++ sym) : acc
    describeSymbol _ _ acc             = acc


-- |Computes a set of referenced symbols
referencedSymbols :: [Statement] -> Set String
referencedSymbols = Set.fromList . concat . map references
  where
    -- extract symbol references from the program statements
    references st = case st of
      Instruction _ (Operand _ (Symbol s _)) -> [s]
      Data _ vs                              -> concat $ map (\v -> case v of { Symbol s _ -> [s]; _ -> []}) vs
      _                                      -> []


-- |Computes a set of undefined symbols
undefinedSymbols :: SymbolTable -> [Statement] -> Set String
undefinedSymbols t = Set.filter (\s -> not $ Map.member s t) . referencedSymbols


-- |Computes a set of unused symbols
unusedSymbols :: SymbolTable -> [Statement] -> Set String
unusedSymbols t stmts = Set.filter (\s -> Set.notMember s $ referencedSymbols stmts) $ Map.keysSet t


-- |Computes a list of illegal instruction/addressingmode combinations in the programm
illegalInstructions :: [Statement] -> [String]
illegalInstructions = nub . foldr illegal [] . concat . map instructions
  where
    -- accumulate symbol definitions
    illegal (m, o) acc =
      if isIllegalInstruction m o
        then (show m ++ " " ++ show o):acc
        else acc

    -- extract instructions from the program statements
    instructions st = case st of
      Instruction m (Operand o _) -> [(m,o)]
      _                           -> []


-- |Remove unused symbol definitions from a program
stripUnused :: Set String -> [Statement] -> [Statement]
stripUnused unused = filter uselessDefinition
  where
    uselessDefinition st = case st of
      Equ k _ -> Set.notMember k unused
      Label l -> Set.notMember l unused
      _       -> True


-- |A chunk is portion of a program consisting of a start address and a list of statements
data Chunk = Chunk Int [Statement] deriving (Eq,Show)

-- |Chunks are sorted according their start address
instance Ord Chunk where
  (Chunk i1 _) `compare` (Chunk i2 _) = i1 `compare` i2


-- |Split a program into a list of chunks sorted by ascending start address
sortedChunks :: [Statement] -> [Chunk]
sortedChunks = sort . chunks . (:) (Org $ Constant 0)
  where
    chunks []    = []
    chunks stmts =
      let (org,rest) = span isOrg stmts in
      let (prg,rest') = break isOrg rest in
      let (Org (Constant ofs)) = last org in
        (Chunk ofs prg):chunks rest'

    isOrg (Org _) = True
    isOrg _       = False


-- |Map a function over the instructions of a chunk while also providing the correct pc for each address
optmapChunk :: (Int -> Statement -> Maybe a) -> Chunk -> [a]
optmapChunk f (Chunk address statements) =  fst $ foldl' mapStatement ([], address) statements
  where
    mapStatement (res, addr) st =
      ( case f addr st of
          Just r  -> r:res
          Nothing ->   res
      , addr + encodeLength st
      )

    encodeLength st = case st of
      (Instruction _ (Operand a _)) -> 1 + operandLength a
      (Data s vs)                   -> s * length vs
      _                             -> 0


-- |Computes an updated symboltable that contains resolved addresses for all labels
resolveLabels :: SymbolTable -> [Chunk] -> SymbolTable
resolveLabels table = foldr insertLabel table . concat . map (optmapChunk resolveLabel)
  where
    -- Insert a resolved label into a symbol table
    insertLabel (l, addr) = Map.insert l addr

    -- Collect all labels together with their location
    resolveLabel addr (Label l) = Just (l, Just addr)
    resolveLabel _ _ = Nothing


-- |Replace all symbolic values in a program with their corresponding integer values
resolveReferences :: SymbolTable -> [Chunk] -> [Chunk]
resolveReferences t = map elabChunk
  where
    elabChunk (Chunk addr stmts) = Chunk addr $ map elabStatement stmts

    elabStatement st = case st of
      Instruction m (Operand a v) -> Instruction m (Operand a $ elabValue v)
      Data s vs                   -> Data s $ map elabValue vs
      _                           -> st

    elabValue v = case v of
      Constant _ -> v
      Symbol s o -> case Map.lookup s t of
                      Just (Just c) -> Constant $ toWord (c+o)
                      _             -> v

    toWord i | i < 0     = toWord $ i+65536
             | otherwise = i `mod` 65536


-- |Searches for relative branches where the distance to the target address is out of range
overlongBranches :: [Chunk] -> [String]
overlongBranches = concat . map (optmapChunk check)
  where
    check addr (Instruction m (Operand Relative (Constant target))) =
      case target - addr - 2 of
        d | d < -127 || d > 128 -> Just $ printf "%s from $%04x to $%04x, distance is %d bytes" (show m) addr target d
        _                       -> Nothing

    check _ _ = Nothing


-- |An assembled program chunk has a start address and a stream of byte values
data AsmChunk = AsmChunk Int [Word8] deriving (Eq,Show)


-- |Assemble a program
assemble :: [Chunk] -> [AsmChunk]
assemble = map assemble'
  where
    assemble' chunk@(Chunk address _) = AsmChunk address $ concat . reverse . optmapChunk encodeStatement $ chunk

    -- encode instruction and data statements
    encodeStatement addr (Instruction m (Operand a (Constant v))) = encodeInstruction addr m a v
    encodeStatement _ (Data s vs) = Just $ concat $ map (encodeValue s) vs
    encodeStatement _ _ = Nothing

    -- encode a single data value
    encodeValue s (Constant c) = encodeOperand s c
    encodeValue _ _ = []


-- |Check for overlaps between adjacent chunks
overlappingParts :: [AsmChunk] -> [String]
overlappingParts = fst . foldr check ([], maxBound::Int)
  where
    check (AsmChunk start bytes) (errs, addr) =
      ( case start + length bytes - addr of
          d | d > 0 -> (printf "'ORG $%04x' overlaps with %d bytes from 'ORG $%04x'" addr d start) : errs
          _         -> errs
      , start
      )


-- |Merge assembled chunks into a single chunk
merge :: [AsmChunk] -> AsmChunk
merge = foldl1' merge'
  where
    merge' a (AsmChunk _ []) = a
    merge' (AsmChunk _ []) a = a
    merge' (AsmChunk a0 s0) (AsmChunk a1 s1) = (AsmChunk a0 $ s0 ++ (replicate (a1 - a0 - length s0) 0) ++ s1)
