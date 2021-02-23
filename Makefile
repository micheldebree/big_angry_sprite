# http://www.theweb.dk/KickAssembler
C64JASM=c64jasm
# https://sourceforge.net/projects/c64-debugger/
DEBUGGER=/Applications/C64\ Debugger.app/Contents/MacOS/C64\ Debugger
# DEBUGGER=start "" "C:\Program Files\C64Debugger.exe" # on Windows
# https://bitbucket.org/magli143/exomizer/wiki/Home
EXOMIZER=/usr/local/bin/exomizer

.PHONY: %.debug
.PRECIOUS: %.exe.prg

# Compile assembly files with C64Jasm
%.prg: %.asm $(KICKASS)
	$(C64JASM) "$<" --c64debugger-symbols-file "$*.dbg" --out "$@"

# Build a crunched version
%.exe.prg: %.prg
	exomizer sfx basic "$<" -o "$*.exe.prg"
	x64sc "$*.exe.prg"

# Build and debug
%.debug: %.prg
	$(DEBUGGER) -prg "$*.prg" -pass -unpause -wait 3000 -autojmp -layout 9

main.prg:

clean:
	rm -f *.prg
	rm -f *.exe.prg
	rm -f *.sym
	rm -f *.vs
	rm -f *.dbg
	rm -f *.d64


