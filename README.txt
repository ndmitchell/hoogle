README FOR HOOGLE
=================

A Haskell API search. To invoke it type

	hoogle "[a] -# [b]"

Where -# is used instead of -> so it does not conflict with the console.


Web Version
-----------

A web version is available at http://www.haskell.org/hoogle

All the appropriate documentation/credits/reference material is on the Haskell wiki at
http://www.haskell.org/haskellwiki/Hoogle


Building
--------

To build the source type "ghc --make" on the files.

Folders
-------

The folders in the distribution, and their meaning are:

data - programs that generate a hoogle data file
docs - documentation on hoogle
src  - source code to the hoogle front ends, and the main code
test - regression tests
web  - additional front end stuff for the web module
