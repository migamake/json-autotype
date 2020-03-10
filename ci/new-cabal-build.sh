#!/bin/bash

source ci/common.sh


mkdir -p hackage-docs bin sdist
for PKG in "${PKGS[@]}"; do
  (cd ${PKG};
   message "Prepare release artifacts for ${PKG}"
   mkdir -p bin sdist
   cabal new-install --bindir=../bin/
   cabal new-sdist   --builddir=../
   cabal new-haddock --builddir=../hackage-docs --for-hackage
  )
done;

message "Run on example"
cabal new-exec json-autotype -- json-autotype/test/colors.json


