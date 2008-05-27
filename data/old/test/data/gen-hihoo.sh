#!/bin/sh

if [ "x$1" = "x" ] ; then echo "usage: $0 FILE" ; exit 1 ; fi

mkdir hihoo
mkdir hihoo/$1
ghc -odir hihoo/$1 -hidir hihoo/$1 -c examples/$1.hs
perl ../../data/hihoo/hihoo.pl hihoo/$1/$1.hi > hihoo/$1.hoo
