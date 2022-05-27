FROM migamake/haskell-build:8.6
#RUN apt-get update
#RUN apt-get install -y ghc cabal-install
RUN  mkdir /src
COPY json-autotype        /src/json-autotype
COPY json-alt             /src/json-alt
COPY run-haskell-module   /src/run-haskell-module
COPY cabal.project        /src/cabal.project
COPY stack.yaml           /src/stack.yaml
WORKDIR /src/json-alt
RUN  ls *.cabal
RUN  cabal v2-update
RUN  cabal v2-install -j --dependencies-only --keep-going --lib || (sleep 60; cabal v2-install -j --dependencies-only --lib)
RUN  cabal v2-install -j --lib
WORKDIR /src/run-haskell-module
RUN  ls *.cabal
RUN  cabal v2-install -j --dependencies-only --keep-going --lib || (sleep 60; cabal v2-install -j --dependencies-only --lib)
RUN  cabal v2-install -j --lib
WORKDIR /src/json-autotype
RUN  ls *.cabal
RUN  cabal v2-install -j --dependencies-only --keep-going --lib || (sleep 60; cabal v2-install -j --dependencies-only --lib)
COPY README.md            /src/README.md
RUN  cabal v2-install -j exe:json-autotype --symlink-bindir=/usr/bin
RUN  mkdir /workdir
WORKDIR /workdir
ENTRYPOINT ["/usr/bin/json-autotype"]

