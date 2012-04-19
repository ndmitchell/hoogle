Hoogle - a Haskell API search.

This repo is being migrated to git.

Command Line Version
--------------------

To invoke Hoogle type:

    hoogle "[a] -> [b]"

Note the quotes, otherwise you will redirect the output to the file [b].

To ensure you have data files for the Hackage modules, you will first need to
type:

    hoogle data

Which will download and build Hoogle databases.


Web Version
-----------

A web version is available at http://www.haskell.org/hoogle

All the appropriate documentation/credits/reference material is on the
Haskell wiki at http://www.haskell.org/haskellwiki/Hoogle


Folders
-------

The folders in the distribution, and their meaning are:

data - tools to generate a hoogle data file
docs - documentation on hoogle
misc - presentations, icons, emacs scripts, logos
src  - source code
web  - additional resources for the web front end (css, jpg etc.)
