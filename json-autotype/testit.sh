#!/bin/bash

#EXE=GenerateJSONParser
EXE=dist/build/json-autotype/json-autotype
#EXE=json-autotype
#SRC=${EXE}.hs

#GHCOPTS=-package=aeson
#GHCOPTS=-package=aeson-0.9.0.1
# TODO: add ParseJSON.hs
#ghc --make ${SRC} -o ${EXE} && \
#cabal build
for i in `find test/ examples/ -iname '*.json'`; do
  basename $i
  echo stack exec -- ${EXE} $i
  OUT=`basename $i .json`.hs
  time stack exec -- ${EXE} $i --outputFilename ${OUT} && stack exec -- ghc ${GHCOPTS} ${OUT} || exit 1
  stack exec -- runghc -- ${GHCOPTS} ${OUT} ${i} || exit 2
done
echo Finished
