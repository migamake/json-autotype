#!/bin/bash

rm `find . -iname '*.dyn_o' -or -iname '*.dyn_hi' -or -iname '*.o' -or -iname '*.p_o' -or -iname '*.p_hi' -or -iname '*.hi'`

rm JSONTypes.hs

for i in test/*.json; do
  OUT=`basename $i .json`.hs
  echo rm -f $OUT
  rm -f $OUT
done
echo Finished
