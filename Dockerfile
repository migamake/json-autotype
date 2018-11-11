FROM ubuntu:latest
RUN apt update
RUN apt install -y ghc cabal-install
RUN mkdir /src
COPY GenerateJSONParser.hs /src/
COPY GenerateTestJSON.hs   /src/
COPY CommonCLI.hs          /src/
COPY Setup.hs              /src/
COPY json-autotype.cabal   /src/
COPY test                  /src/test
COPY Data                  /src/Data
COPY README.md             /src/
COPY LICENSE               /src/
COPY changelog.md          /src/
WORKDIR /src
RUN cabal update
RUN cabal install -j --dependencies-only
RUN cabal install -j
ENTRYPOINT ["cabal", "exec", "json-autotype"]

