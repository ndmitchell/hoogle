if not exist obj mkdir obj
if not exist obj\norm mkdir obj\norm
if not exist obj\prof mkdir obj\prof

if "%1" == "prof" ghc -prof -auto-all --make -odir obj\prof -hidir obj\prof Main.hs -o hooglep.exe
if "%1" == "" ghc --make -odir obj\norm -hidir obj\norm Main.hs -o hoogle.exe
