#!/bin/bash

source ci/common.sh

message "Dependencies"
cabal update
cabal install --dependencies-only --enable-tests

message "Build"
cabal v1-configure --enable-tests --allow-newer
cabal v1-build
cabal v1-test

message "Prepare release artifacts"
mkdir -p bin sdist
cabal install --bindir=bin/
cabal sdist   --builddir=sdist/
cabal haddock --builddir hackage-docs --for-hackage

message "Run on own source"
bin/homplexity-cli lib/
