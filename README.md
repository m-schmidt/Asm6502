# Asm6502

Asm6502 is an assembler for the MOS Technology 6502. The generated `PRG` output files can be loaded into an C64 emulator.


## Commandline Syntax

The commandline syntax for Asm6502 is:

> asm6502 [options] <input-file>

Possible options are:

 *  `-h`, `-?` or `--help`
    Print a help message and quit.

 *  `-v` or `--version`
    Print the version information and quit.

 *  `-s` or `--symtab`
    For debugging purposes. Prints information about resolved symbol addresses.

 *  `-d` or `--dparse`
    For debugging purposes. The input program, as interpreted by the parser, is printed into a file in addition to the normal operation. If the input file is named e.g. `input.asm` the debug output will be written to `input.parsed.asm`.

 *  `-o FILE` or `--output=FILE`
    Set the name of the output file for the assembled program. If not specified, an input file like e.g. `input.asm` is by default assembled to `input.prg` or `input.hex` depending on the selected output format.

 *  `-f FORMAT` or `--format=FORMAT`
    Set the output format of the assembler to either `PRG` (a simple binary format) or `HEX` (a hexdump). If not specified, the default output format is `PRG`.

### @file Support

Before parsing the commandline, any argument starting with an `@` symbol is interpreted as a file path. The argument on the command line is replaced with the contents of this file - each line as a new argument. Note, that there is no recursive replacement. For example, assume a file named `afile` with the following content:

    -f
    HEX
    -d
    -o
    out.hex

Then a commandline like:

> asm6502 @/path/to/afile input.asm


will be transformed to:

> asm6502 -f HEX -d -o out.hex input.asm


## Supported Assembler Directives

Asm6502 supports the following directives:

 * `Name EQU Constant`
    Defines a symbolic `Name` as an alias for a constant value. Example:
    > VSCRBASE EQU $7000

 * `ORG Constant`
    Defines the target address where Asm6502 will continue assembling its output to. Example:
    > ORG $2000

 * `Labelname:`
    Define a new label.

 * `HEXDATA <list of hexadecimal values>`
    The list of hexadecimal values is not modified and forwarded to the assembler output at the current address. Example:
    > HEXDATA $30 $31 $32 $33 $34 $35 $36 $37

 * `STRING "stringliteral"`
    Like `HEXDATA` the given ASCII string is directly forwarded to the assembler output at the current address. Example:
    > STRING "ABCD"

    is equivalent to:

    > HEXDATA $41 $42 $43 $44

 * `CBMSTRING "stringliteral"`
    Like `STRING` but lower case letters (ASCII $61 to $7A) are mapped to the range from $01 to $1A. Example:
    > CBMSTRING "abcd"

    is equivalent to:

    > HEXDATA $01 $02 $03 $04

 * `BYTE Bytevalue`
    Put a single byte value (8 bits) to the assembler output. Example:
    > BYTE $a5

 * `WORD Wordvalue`
    Put a single word value (16 bits) to the assembler output. Encoding is little endian. Example:
    > WORD $a5b6

 * `INCLUDE-PRG "filename"`
    Read the specified file in `PRG` format and add its contents directly to the assembler output. The specified filename can be an absolute or relative file path. The latter is interpreted as relative to the input file of Asm6502. Example:
    > INCLUDE-PRG "/path/to/a/input.prg"


## Syntax for Constant Values

 * Binary values start with a percent sign, e.g. `%1110`.
 * Octal values start with a zero, e.g. `016`.
 * Decimal values start with a non-zero decimal digit, e.g. `14`.
 * Hexadecimal values start with a dollar sign, e.g. `$0e`.
