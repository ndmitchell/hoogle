#
# Simple Makefile for tests
#

GHC=            ghc
HC_OPTS=        -O

.PHONY: all

all: runtests

runtests: 
	$(GHC) $(HC_OPTS) --make -o $@ runtests.hs

clean:
	rm -rf hihoo hadhtml runtests
	find . -name '*.hi' -o -name '*.o' | xargs rm -rf
