-- | Module definining the 6502 architecture.
module Arch6502 where

import Data.Bits
import Data.Word


-- |All supported 6502 Mnemonics
data Mnemonic = ADC   -- ^ Add with carry
              | AND   -- ^ And (with accumulator)
              | ASL   -- ^ Arithmetic shift left
              | BCC   -- ^ Branch on carry clear
              | BCS   -- ^ Branch on carry set
              | BEQ   -- ^ Branch on equal
              | BIT   -- ^ Bit test
              | BMI   -- ^ Branch on minus
              | BNE   -- ^ Branch on not equal
              | BPL   -- ^ Branch on plus
              | BRK   -- ^ Interrupt
              | BVC   -- ^ Branch on overflow clear
              | BVS   -- ^ Branch on overflow set
              | CLC   -- ^ Clear carry
              | CLD   -- ^ Clear decimal
              | CLI   -- ^ Clear interrupt disable
              | CLV   -- ^ Clear overflow
              | CMP   -- ^ Compare (with accumulator)
              | CPX   -- ^ Compare with X
              | CPY   -- ^ Compare with Y
              | DEC   -- ^ Decrement
              | DEX   -- ^ Decrement X
              | DEY   -- ^ Decrement Y
              | EOR   -- ^ Exclusive or (with accumulator)
              | INC   -- ^ Increment
              | INX   -- ^ Increment X
              | INY   -- ^ Increment Y
              | JMP   -- ^ Jump
              | JSR   -- ^ Jump subroutine
              | LDA   -- ^ Load accumulator
              | LDX   -- ^ Load X
              | LDY   -- ^ Load Y
              | LSR   -- ^ Logical shift right
              | NOP   -- ^ No operation
              | ORA   -- ^ Or with accumulator
              | PHA   -- ^ Push accumulator
              | PHP   -- ^ Push processor status
              | PLA   -- ^ Pull accumulator
              | PLP   -- ^ Pull processor status
              | ROL   -- ^ Rotate left
              | ROR   -- ^ Rotate right
              | RTI   -- ^ Return from interrupt
              | RTS   -- ^ Return from subroutine
              | SBC   -- ^ Subtract with carry
              | SEC   -- ^ Set carry
              | SED   -- ^ Set decimal
              | SEI   -- ^ Set interrupt disable
              | STA   -- ^ Store accumulator
              | STX   -- ^ Store X
              | STY   -- ^ Store Y
              | TAX   -- ^ Transfer accumulator to X
              | TAY   -- ^ Transfer accumulator to Y
              | TSX   -- ^ Transfer stack pointer to X
              | TXA   -- ^ Transfer X to accumulator
              | TXS   -- ^ Transfer X to stack pointer
              | TYA   -- ^ Transfer Y to accumulator
              deriving (Eq,Enum,Bounded,Show,Read)


-- |List of all 6502 Mnemonics
allMnemonics :: [Mnemonic]
allMnemonics = [minBound..maxBound]


-- |Predicate to filter relative branch instructions
isBranch :: Mnemonic -> Bool
isBranch m = opCode m Relative /= Nothing


-- |Predicate to filter rotate and shift instructions
isShiftRotate :: Mnemonic -> Bool
isShiftRotate m = opCode m Accumulator /= Nothing


-- |6502 Addressing Modes
data AddressingMode = Absolute          -- ^ Absolute addressing: $A5B6
                    | AbsoluteX         -- ^ Absolute Indexed with X: $A5B6,X
                    | AbsoluteY         -- ^ Absolute Indexed with X: $A5B6,Y
                    | Accumulator       -- ^ Accumulator: A
                    | Immediate         -- ^ Immediate: #$A5
                    | Implied           -- ^ No operand is specified
                    | IndexedIndirect   -- ^ Indexed Indirect: ($A5,X)
                    | Indirect          -- ^ Indirect ($A5)
                    | IndirectIndexed   -- ^ Indirect Indexed: ($A5),Y
                    | Relative          -- ^ Specified operand must be converted into a relative offset (used for branches)
                    | ZeroPage          -- ^ Zero Page: $A5
                    | ZeroPageX         -- ^ Zero Page Indexed with X: $A5,X
                    | ZeroPageY         -- ^ Zero Page Indexed with Y: $A5,Y
                    deriving (Eq,Enum,Show)


-- |Encode a mnemonic with an operand of a specific addressing mode into a sequence of bytes
encodeInstruction :: Int -> Mnemonic -> AddressingMode -> Int -> Maybe [Word8]
encodeInstruction addr mnemonic mode value =
  -- encoding is the instruction opcode followed by a stream of bytes for the operand
  case opCode mnemonic mode of
    Just code -> Just $ code : encodeOperand (operandLength mode) value'
    _         -> Nothing

  where
    -- convert target address into relative distance for branches
    value' = if mode == Relative then value - addr - 2 else value


-- |Encode an integer value into a little endian sequence of bytes
encodeOperand :: Int -> Int -> [Word8]
encodeOperand 0 _ = []
encodeOperand n v = (fromIntegral $ v .&. 255) : encodeOperand (n-1) (v `shift` (-8))


-- |Returns the number of bytes needed to encode an addressing mode
operandLength :: AddressingMode -> Int
operandLength mode = case mode of
  Accumulator -> 0
  Implied     -> 0
  Absolute    -> 2
  AbsoluteX   -> 2
  AbsoluteY   -> 2
  _           -> 1


-- |Returns the opcode for a combination of instruction mnemonic and addressing mode
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


-- |Check whether a combination of mnemonic and addressing mode is illegal
isIllegalInstruction :: Mnemonic -> AddressingMode -> Bool
isIllegalInstruction = ((Nothing ==) . ) . opCode
