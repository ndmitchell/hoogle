#
# Simple Makefile for command line tool
#

GHC=            ghc
HC_OPTS=        -O

.PHONY: all

all: hoogle

hoogle: 
	cd src && $(GHC) $(HC_OPTS) --make -o a.out CmdLine.hs
	mv src/a.out ./$@
	cp src/hoogle.txt .


clean:
	rm -rf hoogle
	find src -name '*.hi' -o -name '*.o' -exec rm -rf {} \;
