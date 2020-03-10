#!/bin/bash

message () {
  echo -e "\e[1m\e[33m${*}\e[0m"
}
set -o verbose
set -o xtrace
set -o pipefail
set -o errexit
set -o nounset

message "Versions"
cabal --version
ghc   --version
hpack --version

message "Run hpack"
for i in json-autotype json-alt run-haskell-module; do
  (cd $i; hpack);
done
