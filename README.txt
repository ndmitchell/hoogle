README FOR HOOGLE
=================

A Haskell API search. To invoke it type

	hoogle "[a] -> [b]"

Note the quotes, otherwise you will redirect the output to the file [b].


Web Version
-----------

A web version is available at http://www.haskell.org/hoogle

All the appropriate documentation/credits/reference material is on the Haskell wiki at
http://www.haskell.org/haskellwiki/Hoogle


Building
--------

To build the source type follow the standard Cabal procedure:

$ runhaskell Setup configure
$ runhaskell Setup build
$ runhaskell Setup install

Folders
-------

The folders in the distribution, and their meaning are:

data - programs that generate a hoogle data file
docs - documentation on hoogle
src  - source code to the hoogle front ends, and the main code
test - regression tests
web  - additional front end stuff for the web module
