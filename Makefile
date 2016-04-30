.PHONY: all
all:
	ghc --make -O -Wall -o asm6502 Main.hs
	strip asm6502

.PHONY: clean
clean:
	rm -f *.o *.hi asm6502
