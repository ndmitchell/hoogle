#
# Simple Makefile for command line tool
#

GHC=            ghc
HC_OPTS=        -O

.PHONY: all

all: hoogle

hoogle: 
	cd src && $(GHC) $(HC_OPTS) --make -o $@ CmdLine.hs
	mv src/hoogle .
	cp src/hoogle.txt .


clean:
	rm -rf hoogle
	find src -name '*.hi' -o -name '*.o' -exec rm -rf {} \;
