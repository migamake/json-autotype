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

EXAMPLES:
=========

The most simple example:
```
    {
        "colorsArray":[{
                "colorName":"red",
                "hexValue":"#f00"
            },
            {
                "colorName":"green",
                "hexValue":"#0f0"
            },
            {
                "colorName":"blue",
                "hexValue":"#00f"
            }
        ]
    }
```

It will produce the module with the following datatypes and TH calls for JSON parser derivations:
```
    data ColorsArray = ColorsArray {
        colorsArrayHexValue    :: Text,
        colorsArrayColorName :: Text
      } deriving (Show,Eq)

    data TopLevel = TopLevel {
        topLevelColorsArray :: ColorsArray
      } deriving (Show,Eq)
```
Note that attribute names match the names of JSON dictionary keys.

Another example with ambiguous types:
```
    {
        "parameter":[{
                "parameterName":"apiVersion",
                "parameterValue":1
            },
            {
                "parameterName":"failOnWarnings",
                "parameterValue":false
            },
            {
                "parameterName":"caller",
                "parameterValue":"site API"
            }]
    }
```
It will produce quite intuitive result (plus extra parentheses, and class derivations):

```
    data Parameter = Parameter {
        parameterParameterValue :: Either Bool (Either Int Text),
        parameterParameterName :: Text
      }

    data TopLevel = TopLevel {
        topLevelParameter :: Parameter
      }
```

Real-world use case examples are provided in the package [source repository](https://github.com/mgajda/json-autotype/tree/master/test).
