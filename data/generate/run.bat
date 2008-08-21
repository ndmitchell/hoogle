if not exist obj mkdir obj
ghc --make Main -odir obj -hidir obj -o obj/main && obj\main %*
