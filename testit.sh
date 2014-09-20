#!/bin/bash

# TODO: add ParseJSON.hs
ghc --make Test.hs -o Test && \
for i in test/*.json; do
  basename $i
  echo ./Test $i
  OUT=`basename $i .json`.hs
  time ./Test $i --filename ${OUT} && ghc ${OUT} || exit 1
done
echo Finished
