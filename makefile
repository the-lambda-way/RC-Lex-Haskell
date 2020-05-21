IN_FILES=$(wildcard test/*.t)
OUT_FILES=$(IN_FILES:.t=.out)

lex: lex.hs
	ghc lex.hs -o lex

.PHONY: test clean

test: lex $(OUT_FILES)

%.out: %.t
	./lex < $^ > $@
	diff $*.expected $@

clean:
	rm *.hi *.o lex test/*.out