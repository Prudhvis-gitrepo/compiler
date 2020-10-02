# Makefile to build all standalone programs.
#
%.lex.sml: %.lex
	mllex $<

%.grm.sml : %.grm
	mlyacc $<

all: tigc

.PHONY: all clean test

clean:
	rm -f tigc
	rm -f *.lex.sml

tigh: tokens.sml tiger.lex.sml tiger.mlb tigh.sml
	mlton -output tigh tiger.mlb

tigc: tiger.sml tiger.mlb expr.grm.sml tiger.lex.sml indent.sml    
	mlton -output tigc tiger.mlb

test: tigc
	${CURDIR}/tigc test.inp

