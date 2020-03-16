#!/bin/bash

set -euo pipefail

cabal update
cabal configure
cabal sdist
cabal upload --username="$HACKAGE_USER" --password="$HACKAGE_PASSWORD"    dist/homplexity-*[0-9].tar.gz
cabal haddock --for-hackage
cabal upload --username="$HACKAGE_USER" --password="$HACKAGE_PASSWORD" -d dist/homplexity-*-docs.tar.gz
