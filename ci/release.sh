#!/bin/bash

source ci/common.sh

message "Packages"
for FILENAME in sdist/*.tar.gz; do
  message "Uploading ${FILENAME}"
  cabal upload ${*} --username="$HACKAGE_USER" --password="$HACKAGE_PASSWORD"    "${FILENAME}" || echo "Already uploaded? ${FILENAME}";
done

message "Documentation"
for FILENAME in hackage-docs/*.tar.gz; do
  message "Uploading ${FILENAME}"
  cabal upload ${*} --username="$HACKAGE_USER" --password="$HACKAGE_PASSWORD" -d "${FILENAME}" || echo "Already uploaded? ${FILENAME}";
done
