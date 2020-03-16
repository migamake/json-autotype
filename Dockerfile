FROM ubuntu:latest
RUN apt-get update
RUN apt-get install -y ghc cabal-install
RUN mkdir /src
COPY json-autotype      /src/json-autotype
COPY json-alt           /src/json-alt
COPY run-haskell-module /src/run-haskell-module
COPY stack.yaml            /src/stack.yaml
WORKDIR /src/json-alt
RUN cabal update
RUN cabal install -j --dependencies-only --keep-going || (sleep 60; cabal install -j --dependencies-only)
RUN cabal install -j
WORKDIR /src/run-haskell-module
RUN cabal install -j --dependencies-only --keep-going || (sleep 60; cabal install -j --dependencies-only)
RUN cabal install -j
WORKDIR /src/json-autotype
RUN cabal install -j --dependencies-only --keep-going || (sleep 60; cabal install -j --dependencies-only)
RUN cabal install -j
RUN mkdir /workdir
WORKDIR /workdir
ENTRYPOINT ["cabal", "exec", "json-autotype", "--"]

