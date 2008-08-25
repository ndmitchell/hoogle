echo Building Hoogle @York
cd /tmp/ndm/hoogle
darcs pull --all
runhaskell Setup configure --with-compiler=ghc-6.6.1 --with-hc-pkg=ghc-pkg-6.6.1 --user --datadir=res --datasubdir=.
runhaskell Setup build
echo Finished building
