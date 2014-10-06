json-autotype
=============
Takes a JSON format input, and generates automatic Haskell type declarations.
*Goal is to also generate parser, and pretty-printer instances.*

It uses union type unification. Types inferred may be automatically trimmed and unified
using attribute set matching.

I should probably write a short paper to explain the methodology.

[![Build Status](https://api.travis-ci.org/mgajda/json-autotype.png?branch=master)](https://travis-ci.org/mgajda/json-autotype)
[![Hackage](https://budueba.com/hackage/json-autotype)](https://hackage.haskell.org/package/json-autotype)

Details on official releases will be on [Hackage](https://hackage.haskell.org/package/json-autotype)

USAGE:
======
After installing with `cabal install json-autotype`, you might generate stub code for the parser:

    json-autotype input.json -o MyFormat.hs

Then you might test the parser by running it on an input file:

    runghc MyFormat.hs input.json

If everything is correct, then feel free to inspect the data structure generated automatically for you!
The goal of this program is to make it easy for users of big JSON APIs to generate entries from
example data.

