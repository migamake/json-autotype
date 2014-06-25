#!/bin/bash

# TODO: add ParseJSON.hs
ghc Test.hs -o Test && \
for i in test/*.json; do
  basename $i
  echo ./Test $i
  OUT=`basename $i .json`.hs
  time ./Test $i --output ${OUT} && ghc ${OUT} || exit 1
done
echo Finished
