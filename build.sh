#!/bin/bash

cabal build
cabal test
cabal haddock
cabal sdist
