#!/bin/bash

#EXE=GenerateJSONParser
EXE=dist/build/json-autotype/json-autotype
SRC=${EXE}.hs

#GHCOPTS=-package=aeson
#GHCOPTS=-package=aeson-0.9.0.1
# TODO: add ParseJSON.hs
#ghc --make ${SRC} -o ${EXE} && \
#cabal build
for i in `find test/ examples/ -iname '*.json'`; do
  basename $i
  echo ./${EXE} $i
  OUT=`basename $i .json`.hs
  time ./${EXE} $i --outputFilename ${OUT} && cabal exec ghc -- ${GHCOPTS} ${OUT} || exit 1
  cabal exec -- runghc -- ${GHCOPTS} ${OUT} ${i} || exit 2
done
echo Finished
