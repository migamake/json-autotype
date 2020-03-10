#!/bin/bash

source ci/common.sh


mkdir -p hackage-docs bin sdist
for PKG in "${PKGS[@]}"; do
  (cd ${PKG};
   message "Prepare release artifacts for '${PKG}'"
   mkdir -p bin sdist
   if [ "$PKG" == "json-autotype" ]; then
     message "Installing executables"
     cabal new-install --bindir=../bin/
     ls -lR ../bin
   else
     message "Installing package ${PKG}"
     cabal new-install
   fi;
   cabal new-sdist   --builddir=../
   cabal new-haddock --builddir=../hackage-docs --haddock-for-hackage
  )
done;

message "Run on example"
ls -lR bin/
bin/json-autotype -- json-autotype/test/colors.json


