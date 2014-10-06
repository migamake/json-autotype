#!/bin/bash

EXE=GenerateJSONParser
SRC=${EXE}.hs

# TODO: add ParseJSON.hs
ghc --make ${SRC} -o ${EXE} && \
for i in test/*.json; do
  basename $i
  echo ./${EXE} $i
  OUT=`basename $i .json`.hs
  time ./${EXE} $i --filename ${OUT} && ghc ${OUT} || exit 1
done
echo Finished
