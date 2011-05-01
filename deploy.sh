#!/bin/sh
set -e
set -u
echo Deploying on server
set +u
source ~/.profile
set -u
cd hoogle
darcs pull --all
cabal configure --datadir=/srv/web/haskell.org/hoogle/ --datasubdir=datadir -O2
cabal build
nice dist/build/hoogle/hoogle data default all ghc --redownload --datadir=/srv/web/haskell.org/hoogle/datadir2
cp datadir/resources /srv/web/haskell.org/hoogle/res2
cp dist/build/hoogle/index2.cgi
