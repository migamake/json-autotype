#!/bin/bash

source ci/cabal-common.sh

mkdir -p hackage-docs bin new-bin sdist

message "Build and installation"
# This is still buggy in Cabal 2.4.x
#cabal new-install --bindir=bin/ json-autotype
# Should work for Cabal 3.2
cabal new-install --installdir=new-bin/ json-autotype
ls -lR bin

message "Preparing source distribution"
cabal new-sdist --output-dir=sdist  "${PKGS[@]}"

message "Documentation distribution"
cabal new-haddock --builddir=hackage-docs --haddock-for-hackage  "${PKGS[@]}"

message "Run on example"
# Need working runModule yet!
#bin/json-autotype json-autotype/test/colors.json --outputFilename=Colors.hs
new-bin/json-autotype json-autotype/test/colors.json

message "Copy binary to bin/"
mkdir -p bin
cp --dereference new-bin/json-autotype bin/json-autotype
ls -lR bin/

