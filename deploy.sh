#!/bin/sh
# NOTE: Use GHC 6.6.1 as haskell.org can't cope with GHC 6.8 binaries
#       given error "timer_create: Operation not supported"
#       And York's GHC 6.6 has a broken time library.

mkdir deploy/res --parents
runhaskell Setup configure --with-compiler=ghc-6.6.1 --with-hc-pkg=ghc-pkg-6.6.1 --user --datadir=res
runhaskell Setup build
cp dist/build/hoogle/hoogle deploy/index.cgi
deploy/index.cgi /convert=hoogle.txt /output=deploy/res/default.hoo
cp src/res/* deploy/res
scp -r deploy/* haskell.org:/haskell/hoogle/beta
