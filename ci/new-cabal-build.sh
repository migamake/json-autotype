#!/bin/bash

source ci/common.sh

message "Dependencies"
cabal update

message "Build"
cabal new-build --enable-tests --allow-newer
cabal new-test --allow-newer

message "Prepare artifacts"
mkdir -p bin sdist
cabal new-install --bindir=bin/ --allow-newer --overwrite-policy=always
cabal new-sdist   --builddir=sdist/
cabal new-haddock --builddir hackage-docs --haddock-for-hackage

message "Test on own source"
cabal new-exec -- homplexity-cli lib/ # path is dist/build/homplexity-cli
