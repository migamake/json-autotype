#!/bin/bash

# TODO: add ParseJSON.hs
ghc Test.hs && \
for i in test/*.json; do
  basename $i
  time ./Test $i > JSONTypes.hs && ghc JSONTypes.hs || exit 1
done
echo Finished
