
LIBSRC = $(shell find src/Language -name \*.hs) $(LEXER) $(GRAMMAR)
TESTSRC = $(shell find Tests -name \*.hs)

LEXER = dist/build/Language/JavaScript/Parser/Lexer.hs
GRAMMAR = dist/build/Language/JavaScript/Parser/Grammar5.hs

GHC = cabal exec -- ghc
GHCFLAGS = -Wall -fwarn-tabs


check : testsuite.exe
	./testsuite.exe

# Fuzzing targets
fuzz-basic :
	FUZZ_TEST_ENV=ci cabal test testsuite

fuzz-comprehensive :
	FUZZ_TEST_ENV=development cabal test testsuite

fuzz-regression :
	FUZZ_TEST_ENV=regression cabal test testsuite

clean :
	find dist/build/ src/ -name \*.{o -o -name \*.hi | xargs rm -f
	rm -f $(LEXER) $(GRAMMAR) $(TARGETS) *.exe

testsuite.exe : testsuite.hs $(LIBSRC) $(TESTSRC)
	$(GHC) $(GHCFLAGS) -O2 -i:src -i:dist/build --make $< -o $@


$(GRAMMAR) : src/Language/JavaScript/Parser/Grammar5.y
	happy $+ -o $@

$(LEXER) : src/Language/JavaScript/Parser/Lexer.x
	alex $+ -o $@
