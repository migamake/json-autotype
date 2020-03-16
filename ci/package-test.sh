#!/bin/bash

source ci/common.sh

message "Updating package index"
cabal update

message "Installing tarballs"
ls sdist/
for PKG in "${PKGS[@]}"; do
cabal install sdist/${PKG}*.tar.gz --bindir=bin/
tar xzf sdist/${PKG}*.tar.gz
done 

message "Running executable"
mkdir -p bin
bin/${EXEC_NAME} json-autotype/test/colors.json
