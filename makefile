TESTS=$(wildcard test/*.t)
EXPECTED:=$(TESTS:.t=.expected)

all: lex

.PHONY: test $(EXPECTED) clean

lex: lex.hs
	ghc lex.hs -o lex

test: $(EXPECTED)

$(EXPECTED): %.expected: %.t lex
	@echo testing $<
	@./lex < $< | diff -u --color $@ -

clean:
	rm *.hi *.o lex
