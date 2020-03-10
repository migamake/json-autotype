#!/bin/bash

source ci/common.sh

#message "Dependencies"
#cabal update
#cabal install --dependencies-only --enable-tests

for PKG in "${PKGS[@]}"; do
  (cd ${PKG};
   message "Build $PKG";
   cabal v1-install --dependencies-only
   cabal v1-configure --enable-tests --allow-newer;
   cabal v1-build;
   cabal v1-test;
   cabal v1-install;
  )
done;

mkdir -p hackage-docs bin sdist
for PKG in "${PKGS[@]}"; do
  (cd ${PKG};
   message "Prepare release artifacts for ${PKG}"
   mkdir -p bin sdist
   cabal v1-install --bindir=../bin/ --reinstall --force-reinstalls
   cabal v1-sdist   --builddir=../sdist/
   cabal v1-haddock --builddir=../hackage-docs --for-hackage
  )
done;

message "Run on example"
bin/json-autotype json-autotype/test/colors.json
