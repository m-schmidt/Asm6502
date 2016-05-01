module Arch6502
  ( Mnemonic(..)
  , allMnemonics
  , isBranch
  , isShiftRotate
  , AddressingMode(..)
  , encodeInstruction
  , encodeOperand
  , operandLength
  , isIllegalInstruction
  )

where

import Data.Bits
import Data.Word


-- 6502 Mnemonics
data Mnemonic = ADC   -- add with carry
              | AND   -- and (with accumulator)
              | ASL   -- arithmetic shift left
              | BCC   -- branch on carry clear
              | BCS   -- branch on carry set
              | BEQ   -- branch on equal
              | BIT   -- bit test
              | BMI   -- branch on minus
              | BNE   -- branch on not equal
              | BPL   -- branch on plus
              | BRK   -- interrupt
              | BVC   -- branch on overflow clear
              | BVS   -- branch on overflow set
              | CLC   -- clear carry
              | CLD   -- clear decimal
              | CLI   -- clear interrupt disable
              | CLV   -- clear overflow
              | CMP   -- compare (with accumulator)
              | CPX   -- compare with X
              | CPY   -- compare with Y
              | DEC   -- decrement
              | DEX   -- decrement X
              | DEY   -- decrement Y
              | EOR   -- exclusive or (with accumulator)
              | INC   -- increment
              | INX   -- increment X
              | INY   -- increment Y
              | JMP   -- jump
              | JSR   -- jump subroutine
              | LDA   -- load accumulator
              | LDX   -- load X
              | LDY   -- load Y
              | LSR   -- logical shift right
              | NOP   -- no operation
              | ORA   -- or with accumulator
              | PHA   -- push accumulator
              | PHP   -- push processor status
              | PLA   -- pull accumulator
              | PLP   -- pull processor status
              | ROL   -- rotate left
              | ROR   -- rotate right
              | RTI   -- return from interrupt
              | RTS   -- return from subroutine
              | SBC   -- subtract with carry
              | SEC   -- set carry
              | SED   -- set decimal
              | SEI   -- set interrupt disable
              | STA   -- store accumulator
              | STX   -- store X
              | STY   -- store Y
              | TAX   -- transfer accumulator to X
              | TAY   -- transfer accumulator to Y
              | TSX   -- transfer stack pointer to X
              | TXA   -- transfer X to accumulator
              | TXS   -- transfer X to stack pointer
              | TYA   -- transfer Y to accumulator
              deriving (Eq,Ord,Enum,Bounded,Show,Read)


-- List of all 6502 Mnemonics
allMnemonics :: [Mnemonic]
allMnemonics = [minBound..maxBound]


-- Predicate for relative branch instructions
isBranch :: Mnemonic -> Bool
isBranch m = opCode m Relative /= Nothing


-- Predicate for rotate and shift instructions
isShiftRotate :: Mnemonic -> Bool
isShiftRotate m = opCode m Accumulator /= Nothing


-- 6502 Addressing Modes
data AddressingMode = Absolute          -- $A5B6
                    | AbsoluteX         -- $A5B6,X
                    | AbsoluteY         -- $A5B6,Y
                    | Accumulator       -- A
                    | Immediate         -- #$A5
                    | Implied           -- (no operand)
                    | IndexedIndirect   -- ($A5,X)
                    | Indirect          -- ($A5)
                    | IndirectIndexed   -- ($A5),Y
                    | Relative          -- (specified operand must be converted into a relative offset)
                    | ZeroPage          -- $A5
                    | ZeroPageX         -- $A5,X
                    | ZeroPageY         -- $A5,Y
                    deriving (Eq,Ord,Enum,Show)


-- Encode a mnemonic with an operand of a specific addressing mode into a sequence of bytes
encodeInstruction :: Int -> Mnemonic -> AddressingMode -> Int -> Maybe [Word8]
encodeInstruction addr mnemonic mode value =

  -- convert target address into relative distance for branches
  let value' = if mode == Relative
                 then value - addr - 2
                 else value in

  -- encoding is the instruction opcode followed by a stream of bytes for the operand
  case opCode mnemonic mode of
    Just code -> Just $ code : encodeOperand (operandLength mode) value'
    _         -> Nothing


-- Encode the least significant bytes of an integer value into a little endian sequence of bytes
encodeOperand :: Int -> Int -> [Word8]
encodeOperand 0 _ = []
encodeOperand n v = (fromIntegral $ v .&. 255) : encodeOperand (n-1) (v `shift` (-8))


-- Returns the number of bytes needed to encode an addressing mode
operandLength :: AddressingMode -> Int
operandLength mode = case mode of
  Accumulator -> 0
  Implied     -> 0
  Absolute    -> 2
  AbsoluteX   -> 2
  AbsoluteY   -> 2
  _           -> 1


-- Returns the opcode for a combination of instruction mnemonic and addressing mode
opCode :: Mnemonic -> AddressingMode -> Maybe Word8
opCode mnemonic mode = opcode mode mnemonic
  where
    opcode m = case m of
      Absolute        -> opcodeAbsolute
      AbsoluteX       -> opcodeAbsoluteX
      AbsoluteY       -> opcodeAbsoluteY
      Accumulator     -> opcodeAccumulator
      Immediate       -> opcodeImmediate
      Implied         -> opcodeImplied
      IndexedIndirect -> opcodeIndexedIndirect
      Indirect        -> opcodeIndirect
      IndirectIndexed -> opcodeIndirectIndexed
      Relative        -> opcodeRelative
      ZeroPage        -> opcodeZeroPage
      ZeroPageX       -> opcodeZeroPageX
      ZeroPageY       -> opcodeZeroPageY

    opcodeAbsolute m = case m of
      ADC -> Just 0x6d
      AND -> Just 0x2d
      ASL -> Just 0x0e
      BIT -> Just 0x2c
      CMP -> Just 0xcd
      CPX -> Just 0xec
      CPY -> Just 0xcc
      DEC -> Just 0xce
      EOR -> Just 0x4d
      INC -> Just 0xee
      JMP -> Just 0x4c
      JSR -> Just 0x20
      LDA -> Just 0xad
      LDX -> Just 0xae
      LDY -> Just 0xac
      LSR -> Just 0x4e
      ORA -> Just 0x0d
      ROL -> Just 0x2e
      ROR -> Just 0x6e
      SBC -> Just 0xed
      STA -> Just 0x8d
      STX -> Just 0x8e
      STY -> Just 0x8c
      _   -> Nothing

    opcodeAbsoluteX m = case m of
      ADC -> Just 0x7d
      AND -> Just 0x3d
      ASL -> Just 0x1e
      CMP -> Just 0xdd
      DEC -> Just 0xde
      EOR -> Just 0x5d
      INC -> Just 0xfe
      LDA -> Just 0xbd
      LDY -> Just 0xbc
      LSR -> Just 0x5e
      ORA -> Just 0x1d
      ROL -> Just 0x3e
      ROR -> Just 0x7e
      SBC -> Just 0xfd
      STA -> Just 0x9d
      _   -> Nothing

    opcodeAbsoluteY m = case m of
      ADC -> Just 0x79
      AND -> Just 0x39
      CMP -> Just 0xd9
      EOR -> Just 0x59
      LDA -> Just 0xb9
      LDX -> Just 0xbe
      ORA -> Just 0x19
      SBC -> Just 0xf9
      STA -> Just 0x99
      _   -> Nothing

    opcodeAccumulator m = case m of
      ASL -> Just 0x0a
      LSR -> Just 0x4a
      ROL -> Just 0x2a
      ROR -> Just 0x6a
      _   -> Nothing

    opcodeImmediate m = case m of
      ADC -> Just 0x69
      AND -> Just 0x29
      CMP -> Just 0xc9
      CPX -> Just 0xe0
      CPY -> Just 0xc0
      EOR -> Just 0x49
      LDA -> Just 0xa9
      LDX -> Just 0xa2
      LDY -> Just 0xa0
      ORA -> Just 0x09
      SBC -> Just 0xe9
      _   -> Nothing

    opcodeImplied m = case m of
      BRK -> Just 0x00
      CLC -> Just 0x18
      CLD -> Just 0xd8
      CLI -> Just 0x58
      CLV -> Just 0xb8
      DEX -> Just 0xca
      DEY -> Just 0x88
      INX -> Just 0xe8
      INY -> Just 0xc8
      NOP -> Just 0xea
      PHA -> Just 0x48
      PHP -> Just 0x08
      PLA -> Just 0x68
      PLP -> Just 0x28
      RTI -> Just 0x40
      RTS -> Just 0x60
      SEC -> Just 0x38
      SED -> Just 0xf8
      SEI -> Just 0x78
      TAX -> Just 0xaa
      TAY -> Just 0xa8
      TSX -> Just 0xba
      TXA -> Just 0x8a
      TXS -> Just 0x9a
      TYA -> Just 0x98
      _   -> Nothing

    opcodeIndexedIndirect m = case m of
      ADC -> Just 0x61
      AND -> Just 0x21
      CMP -> Just 0xc1
      EOR -> Just 0x41
      LDA -> Just 0xa1
      ORA -> Just 0x01
      SBC -> Just 0xe1
      STA -> Just 0x81
      _   -> Nothing

    opcodeIndirect m = case m of
      JMP -> Just 0x6c
      _   -> Nothing

    opcodeIndirectIndexed m = case m of
      ADC -> Just 0x71
      AND -> Just 0x31
      CMP -> Just 0xd1
      EOR -> Just 0x51
      LDA -> Just 0xb1
      ORA -> Just 0x11
      SBC -> Just 0xf1
      STA -> Just 0x91
      _   -> Nothing

    opcodeRelative m = case m of
      BCC -> Just 0x90
      BCS -> Just 0xb0
      BEQ -> Just 0xf0
      BMI -> Just 0x30
      BNE -> Just 0xd0
      BPL -> Just 0x10
      BVC -> Just 0x50
      BVS -> Just 0x70
      _   -> Nothing

    opcodeZeroPage m = case m of
      ADC -> Just 0x65
      AND -> Just 0x25
      ASL -> Just 0x06
      BIT -> Just 0x24
      CMP -> Just 0xc5
      CPX -> Just 0xe4
      CPY -> Just 0xc4
      DEC -> Just 0xc6
      EOR -> Just 0x45
      INC -> Just 0xe6
      LDA -> Just 0xa5
      LDX -> Just 0xa6
      LDY -> Just 0xa4
      LSR -> Just 0x46
      ORA -> Just 0x05
      ROL -> Just 0x26
      ROR -> Just 0x66
      SBC -> Just 0xe5
      STA -> Just 0x85
      STX -> Just 0x86
      STY -> Just 0x84
      _   -> Nothing

    opcodeZeroPageX m = case m of
      ADC -> Just 0x75
      AND -> Just 0x35
      ASL -> Just 0x16
      CMP -> Just 0xd5
      DEC -> Just 0xd6
      EOR -> Just 0x55
      INC -> Just 0xf6
      LDA -> Just 0xb5
      LDY -> Just 0xb4
      LSR -> Just 0x56
      ORA -> Just 0x15
      ROL -> Just 0x36
      ROR -> Just 0x76
      SBC -> Just 0xf5
      STA -> Just 0x95
      STY -> Just 0x94
      _   -> Nothing

    opcodeZeroPageY m = case m of
      LDX -> Just 0xb6
      STX -> Just 0x96
      _   -> Nothing


-- Check whether a combination of mnemonic and addressing mode is illegal
isIllegalInstruction :: Mnemonic -> AddressingMode -> Bool
isIllegalInstruction mnemonic mode = opCode mnemonic mode == Nothing
