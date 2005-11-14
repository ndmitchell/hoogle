README FOR HOOGLE
=================

A Haskell API search. To invoke it type

	hoogle "[a] -# [b]"

Where -# is used instead of -> so it does not conflict with the console.


Web Version
-----------

A web version is available at http://www.cs.york.ac.uk/~ndm/hoogle/


Source Code License
-------------------

This file is part of Hoogle, (c) Neil Mitchell 2004-2005
http://www.cs.york.ac.uk/~ndm/hoogle/

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike License.
To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/2.0/
or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Code contributions received from
    Thomas "Bob" Davie (http://www.cs.kent.ac.uk/people/rpg/tatd2/)
    Donald Bruce Stewart (http://www.cse.unsw.edu.au/~dons/)
    Thomas Jäger
    Gaal Yahas (http://gaal.livejournal.com/)
    Mike Dodds (http://www-users.cs.york.ac.uk/~miked/)


Building
--------

To build the source type "hmake hoogle", you will need hmake, happy and ghc/nhc/hugs


Folders
-------

The folders in the distribution, and their meaning are:

data - programs that generate a hoogle data file
docs - documentation on hoogle
src  - source code to the hoogle front ends, and the main code
test - regression tests
web  - additional front end stuff for the web module
