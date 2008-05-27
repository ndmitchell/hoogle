REM %1 is the name of the file to convert

md hadhtml
md hadhtml\%1
haddock examples\%1.hs --odir=hadhtml\%1 -h
hadhtml hadhtml\%1\%1.html
move hoogle.txt hadhtml\%1.hoo
