#!/bin/bash -e
cabal configure --enable-tests
cabal build
cabal test
