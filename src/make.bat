if not exist obj mkdir obj
if not exist obj\norm mkdir obj\norm
if not exist obj\prof mkdir obj\prof
if not exist obj\opt  mkdir obj\opt
if not exist obj\optprof  mkdir obj\optprof

if "%1" == "prof" ghc -prof -auto-all --make -odir obj\prof -hidir obj\prof Main.hs -o hooglep.exe -i. -i..
if "%1" == "opt" ghc -O --make -odir obj\opt -hidir obj\opt Main.hs -o hoogleo.exe -i. -i..
if "%1" == "" ghc --make -odir obj\norm -hidir obj\norm Main.hs -o hoogle.exe -i. -i..
if "%1" == "optprof" ghc -O -prof -auto-all --make -odir obj\optprof -hidir obj\optprof Main.hs -o hoogleop.exe -i. -i..
