#!/bin/sh
mkdir deploy/res --parents
runhaskell Setup configure
runhaskell Setup build
cp dist/build/hoogle/hoogle deploy/index.cgi
deploy/index.cgi /convert=hoogle.txt /output=deploy/res/default.hoo
cp src/res/* deploy/res
scp -r deploy/* haskell.org:/haskell/hoogle/beta
