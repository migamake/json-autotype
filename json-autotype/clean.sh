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

for JSON in `find examples/ -iname '*.json'` \
            test/*.json; do
  rm -f `basename $JSON .json`.hs;
done

rm -f Test[0-9].hs Test[0-9].json Test[0-9][0-9].hs Test[0-9][0-9].json

#rm -f iPhoneMenu.hs fstab.hs fstab-schema.hs moeDict.hs union.hs od600.hs instructions.hs oligo.hs
#rm -f pcr.hs product-schema.hs productdb.hs BasicSchema.hs youtube.hs

echo Finished
