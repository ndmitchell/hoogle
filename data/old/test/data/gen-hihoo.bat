REM %1 is the name of the file to convert

md hihoo
md hihoo\%1
ghc -odir hihoo\%1 -hidir hihoo\%1 -c examples\%1.hs
perl ..\..\data\hihoo\hihoo.pl hihoo\%1\%1.hi > hihoo\%1.hoo
