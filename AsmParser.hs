module AsmParser
  ( Statement(..)
  , Value(..)
  , Operand(..)
  , parseAssembler
  )

where

import Arch6502
import Data.Char
import Error
import Numeric
import System.FilePath
import Text.Parsec.Char
import Text.ParserCombinators.Parsec


-- Statements for assembly programs
data Statement = Data Int [Value]
               | Equ String Value
               | Include String
               | Instruction Mnemonic Operand
               | Label String
               | Org Value
               deriving (Eq,Show)

-- Instruction operands
data Operand = Operand AddressingMode Value
             deriving (Eq,Show)

-- Constant or symbolic values
data Value = Constant Int
           | Symbol String Int
           deriving (Eq,Show)


-- The main parser function
parseAssembler :: String -> IO [Statement]
parseAssembler filename =
  parseFromFile asmProgram filename >>= either (exitWithError . ("Error: "++) . show) return


-- Parser for assembler programs
asmProgram :: Parser [Statement]
asmProgram = do
  p <- endBy (whiteSpace *> line) endOfLine <* eof
  return $ concat p
  where
    line = do
      l <- asmLabels
      s <- option [] asmStatement
      _ <- option () comment
      return $ l++s


-- Parser for assembler labels
asmLabels :: Parser [Statement]
asmLabels = many (try oneLabel)
  where
    oneLabel = do
      l <- lexeme identifier
      _ <- lexeme (char ':') <?> "label designator"
      return $ Label (map toLower l)


-- Parser for assembler statements which may be directives or instructions
asmStatement :: Parser [Statement]
asmStatement =   try equ
             <|> try org
             <|> try hex
             <|> try ascii
             <|> try cbm
             <|> try byte
             <|> try word
             <|> try include
             <|> instruction
             <?> "assembler directive/instruction"
  where
    equ = do
      k <- lexeme identifier
      _ <- lexeme $ caseString "equ"
      v <- asmValue
      return $ [Equ (map toLower k) v]

    org = do
      _ <- lexeme $ caseString "org"
      v <- asmValue
      return $ [Org v]

    hex = do
      _ <- lexeme $ caseString "hexdata"
      s <- many1 (lexeme $ char '$' *> many1 hexDigit)
      return $ [Data 1 $ map (\v -> Constant $ fromBase 16 v) s]

    ascii = do
      _ <- lexeme $ caseString "string"
      s <- lexeme stringLiteral
      return $ [Data 1 $ map (\v -> Constant $ ord v) s]

    cbm = do
      _ <- lexeme $ caseString "cbmstring"
      s <- lexeme stringLiteral
      return $ [Data 1 $ map (\v -> Constant $ ord v - if isAsciiLower v then 96 else 0) s]

    byte = do
      _ <- lexeme $ caseString "byte"
      v <- asmValueOrSymbol
      return $ [Data 1 [v]]

    word = do
      _ <- lexeme $ caseString "word"
      v <- asmValueOrSymbol
      return $ [Data 2 [v]]

    include = do
      _ <- lexeme $ caseString "include-prg"
      f <- lexeme stringLiteral
      return $ [Include $ makeValid f]

    instruction = do
      m <- asmMnemonic
      p <- option (Operand Implied $ Constant 0) asmOperand
      return $ [Instruction m $ refineOp m p]

    refineOp m p | isShiftRotate m, Operand Implied v  <- p = Operand Accumulator v
                 | isBranch m,      Operand Absolute v <- p = Operand Relative v
                 | otherwise                                = p


-- Parser for instruction mnemonics
asmMnemonic :: Parser Mnemonic
asmMnemonic = do w <- try (lexeme asmMnemonic') <?> "instruction mnemonic"
                 return $ read $ map toUpper w
  where
    asmMnemonic' = (choice $ map (caseString . show) allMnemonics) <* notFollowedBy identLetter


-- Parser for instruction operands with constant or symbolic offsets for all addressing modes
asmOperand :: Parser Operand
asmOperand =   try accumulator
           <|> try absoluteX
           <|> try absoluteY
           <|> try absolute
           <|> try indexedIndirect
           <|> try indirectIndexed
           <|> try indirectMode
           <|> immediate
           <?> "instruction operand"
  where
    absolute = do
      v <- asmValueOrSymbol
      return $ Operand (if isZeroPage v then ZeroPage else Absolute) v

    absoluteX = do
      v <- asmValueOrSymbol <* index "xX"
      return $ Operand (if isZeroPage v then ZeroPageX else AbsoluteX) v

    absoluteY = do
      v <- asmValueOrSymbol <* index "yY"
      return $ Operand (if isZeroPage v then ZeroPageY else AbsoluteY) v

    accumulator = do
      _ <- lexeme $ oneOf "aA" <* notFollowedBy identLetter
      return $ Operand Accumulator (Constant 0)

    immediate = do
      v <- char '#' *> asmValueOrSymbol
      return $ Operand Immediate v

    indirectMode = do
      v <- parens asmValueOrSymbol
      return $ Operand Indirect v

    indexedIndirect = do
      v <- parens (asmValueOrSymbol <* index "xX")
      return $ Operand IndexedIndirect v

    indirectIndexed = do
      v <- parens asmValueOrSymbol <* index "yY"
      return $ Operand IndirectIndexed v

    parens p = between (lexeme $ char '(') (lexeme $ char ')') p
    index i = lexeme (char ',') <* lexeme (oneOf i)

    isZeroPage p | Constant c <- p, c < 256 = True
                 | otherwise                = False


-- Parser for binary, octal, decimal, or hexadecimal values
asmValue :: Parser Value
asmValue =   try binValue
         <|> try octValue
         <|> try decValue
         <|> try hexValue
         <?> "integer constant"
  where
    binValue = do
      v <- lexeme $ char '%' *> many1 (oneOf "01")
      return $ Constant $ fromBase 2 v

    octValue = do
      v <- lexeme $ char '0' *> many1 octDigit
      return $ Constant $ fromBase 8 v

    decValue = do
      v <- lexeme $ many1 digit
      return $ Constant $ fromBase 10 v

    hexValue = do
      v <- lexeme $ char '$' *> many1 hexDigit
      return $ Constant $ fromBase 16 v


-- Parser for binary, octal, decimal, or hexadecimal values or symbols
asmValueOrSymbol :: Parser Value
asmValueOrSymbol =   try asmValue
                 <|> symbol
                 <?> "integer constant or symbolic value"
  where
    symbol = do
      s <- lexeme identifier
      o <- option 0 (try offset)
      return $ Symbol (map toLower s) o

    offset = do
      s <- lexeme $ oneOf "+-"
      Constant o <- asmValue <?> "integer offset"
      return $ if s=='+' then o else -o


-- Convert digit strings of a given base to an integer value.
fromBase :: Int -> String -> Int
fromBase b = fst . head . readInt b ((<b) . digitToInt) digitToInt


-- Parser for line based comments
comment :: Parser ()
comment = char ';' *> (skipMany $ noneOf "\r\n")


-- Parser for identifiers
identStart :: Parser Char
identStart = letter <|> char '_' <?> ""

identLetter :: Parser Char
identLetter = alphaNum <|> char '_' <?> ""

identifier :: Parser String
identifier = do
  p <- identStart
  q <- many identLetter
  return $ p:q


-- Parser for string literals
stringLiteral :: Parser String
stringLiteral = between (char '"') (char '"') (many $ noneOf "\"\n\r")


-- Parser for case insensitive strings
caseString :: String -> Parser String
caseString s = try (mapM (\c -> char (toLower c) <|> char (toUpper c)) s)


-- Parser that skips any whitespace except newline characters
whiteSpace :: Parser ()
whiteSpace = skipMany $ oneOf " \t\v"


-- Parser generator that applies a parser and the whiteSpace parser after that
lexeme :: Parser a -> Parser a
lexeme p = p <* whiteSpace
