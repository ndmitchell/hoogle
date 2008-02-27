
GHC=            ghc
HC_OPTS=        -O

.PHONY: all deploy york haskell

all: hoogle

hoogle: 
	cd src && $(GHC) $(HC_OPTS) --make -o a.out CmdLine.hs
	mv src/a.out ./$@
	cp hoogle.txt .


clean:
	rm -rf hoogle
	find src -name '*.hi' -o -name '*.o' -exec rm -rf {} \;


deploy:
	mkdir -p deploy
	mkdir -p deploy/res
	mkdir -p deploy/haddock
	rm -f deploy/res/lambdabot.*
	wget http://www.cse.unsw.edu.au/~dons/lambdabot/State/where --output-document=deploy/res/lambdabot.txt.gz --no-clobber
	gunzip deploy/res/lambdabot.txt.gz
	cd src && $(GHC) $(HC_OPTS) --make -o ../deploy/index.cgi Web.hs
	cd src && $(GHC) $(HC_OPTS) --make -o ../deploy/hoodoc.cgi Doc.hs
	cp -r web/* deploy
	cp hoogle.txt deploy/res/hoogle.txt
	cp src/Web/res/* deploy/res
	cp src/Doc/res/* deploy/res
	haddock --html --title=Hoogle --odir=deploy/haddock --prologue=docs/haddock.txt src/*.hs src/*/*.hs


york: deploy
	cp -r deploy/* $(HOME)/web/cgi-bin/hoogle
	cd deploy && find * | grep '\.' | grep -v cgi | grep -v txt | xargs -i cp --parents {} $(HOME)/web/res-cgi-bin/hoogle


haskell: deploy
	scp -r deploy/* $(LOGNAME)@haskell.org:/haskell/hoogle
