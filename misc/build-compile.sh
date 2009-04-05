echo Compiling Hoogle
cd /tmp/ndm/hoogle
darcs pull --all
runhaskell Setup configure --user --datadir=res --datasubdir=.
runhaskell Setup build
echo Finished compiling
