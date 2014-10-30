json-autotype
=============
Takes a JSON format input, and generates automatic Haskell type declarations.

Parser and printer instances are derived using [Aeson](http://hackage.haskell.org/package/aeson).

The program uses union type unification to trim output declarations. The types of same attribute tag and similar attribute set, are automatically unified using recognition by attribute set matching. (This option can be optionally turned off, or a set of unified types may be given explicitly.) `Either` alternatives is used to assure that all `JSON` inputs seen in example input file are handled correctly.

I should probably write a short paper to explain the methodology.

[![Build Status](https://api.travis-ci.org/mgajda/json-autotype.png?branch=master)](https://travis-ci.org/mgajda/json-autotype)
[![Hackage](https://budueba.com/hackage/json-autotype)](https://hackage.haskell.org/package/json-autotype)

Details on official releases are on [Hackage](https://hackage.haskell.org/package/json-autotype)

USAGE:
======
After installing with `cabal install json-autotype`, you might generate stub code for the parser:

```
    json-autotype input.json -o MyFormat.hs
```

Then you might test the parser by running it on an input file:

```
    runghc MyFormat.hs input.json
```

If everything is correct, then feel free to inspect the data structure generated automatically for you!
The goal of this program is to make it easy for users of big JSON APIs to generate entries from
example data.

Occasionally you might find a valid JSON for which `json-autotype` doesn't generate a correct parser.
You may either edit the resulting file _and_ send it to the author as a test case for future release.

Patches and suggestions are welcome.
