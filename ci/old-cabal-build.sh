#!/bin/bash

source ci/cabal-common.sh

export CI_GHC_CABAL_STYLE=v1
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

message "Preparing artifacts..."
mkdir -p hackage-docs bin sdist
for PKG in "${PKGS[@]}"; do
  (cd ${PKG};
   message "Prepare release artifacts for ${PKG}"
   cabal v1-install --bindir=../bin/ --reinstall --force-reinstalls
   cabal v1-sdist   --builddir=../sdist/
   # Buggy in v1-* port
   #cabal v1-haddock --builddir=../hackage-docs --for-hackage
  )
done;

message "Check artifacts"
ls -lR sdist bin hackage-docs

message "Run on example"
bin/json-autotype json-autotype/test/colors.json
