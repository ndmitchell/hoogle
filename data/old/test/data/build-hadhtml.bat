pushd ..\..\data\hadhtml
ghc --make Main -o hadhtml.exe
popd
copy ..\..\data\hadhtml\hadhtml.exe hadhtml.exe

