#!/bin/bash

source ci/common.sh

message "Versions"
cabal --version
ghc   --version
hpack --version

message "Run hpack"
for PKG in "${PKGS[@]}"; do
  (cd $PKG; hpack);
done

mkdir -p hackage-docs bin sdist

message "Package index"
cabal update
