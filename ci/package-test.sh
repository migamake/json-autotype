#!/bin/bash

source ci/common.sh

message "Updating package index"
cabal update

message "Installing tarballs"
mkdir -p bin
ls sdist/
for PKG in "${PKGS[@]}"; do
cabal install sdist/${PKG}*.tar.gz --bindir=bin/
tar xzf sdist/${PKG}*.tar.gz
done 

message "Running executable"
ls -alth bin
ls -alth json-autotype*
bin/${EXEC_NAME} json-autotype-*/test/colors.json
