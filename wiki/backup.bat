REM First get the library docs
wget http://www.haskell.org/hawiki/LibraryDocumentation --html-extension --recursive --level=1 --include-directories=hawiki
copy www.haskell.org\hawiki\LibraryDocumentation*.* .

REM And now specific hoogle related pages
wget http://www.haskell.org/hawiki/Hoogle --html-extension
wget http://www.haskell.org/hawiki/Keywords --html-extension
