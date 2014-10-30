#!/bin/bash

rm `find . -iname '*.dyn_o' -or -iname '*.dyn_hi' -or -iname '*.o' -or -iname '*.p_o' -or -iname '*.p_hi' -or -iname '*.hi'`

rm JSONTypes.hs

for EXT in json bad_json auto_error; do 
  for i in test/*.${EXT}; do
    OUT=`basename $i .${EXT}`.hs
    echo rm -f $OUT
    rm -f $OUT
  done
done
echo Finished
