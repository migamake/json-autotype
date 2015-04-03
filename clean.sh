#!/bin/bash

rm `find . -iname '*.dyn_o' -or -iname '*.dyn_hi' -or -iname '*.o' -or -iname '*.p_o' -or -iname '*.p_hi' -or -iname '*.hi'`

rm JSONTypes.hs

for EXT in json bad_json auto_error; do 
  for i in "test/*.${EXT}"; do
    OUT=`basename $i .${EXT}`.hs;
    if [ "$OUT" != "*.hs" ] ; then
      echo rm -f $OUT
      rm -f $OUT
    fi;
  done
done

rm -f Test[0-9].hs Test[0-9].json Test[0-9][0-9].hs Test[0-9][0-9].json

echo Finished

