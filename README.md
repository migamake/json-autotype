json-autotype
=============
Takes a JSON format input, and generates automatic Haskell type declarations.

Parser and printer instances are derived using [Aeson](http://hackage.haskell.org/package/aeson).

The program uses union type unification to trim output declarations. The types of same attribute tag and similar attribute set, are automatically unified using recognition by attribute set matching. (This option can be optionally turned off, or a set of unified types may be given explicitly.) `:|:` alternatives (similar to `Either`) are used to assure that all `JSON` inputs seen in example input file are handled correctly.

I should probably write a short paper to explain the methodology.

[![Build Status](https://api.travis-ci.org/mgajda/json-autotype.svg?branch=master)](https://travis-ci.org/mgajda/json-autotype)
[![Hackage](https://budueba.com/hackage/json-autotype)](https://hackage.haskell.org/package/json-autotype)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/json-autotype.svg?style=flat)](http://packdeps.haskellers.com/feed?needle=json-autotype)

Details on official releases are on [Hackage](https://hackage.haskell.org/package/json-autotype)

USAGE:
======
After installing with `cabal install json-autotype`, you might generate stub code for the parser:

```
    json-autotype input1.json ... inputN.json -o MyFormat.hs
```

Then you might test the parser by running it on an input file:

```
    runghc MyFormat.hs input.json
```

At this point you may see data structure generated automatically for you.
The more input files you give to the inference engine `json-autotype`,
the more precise type description will be.

Algorithm will also suggest which types look similar, based on a set of attribute names,
and unify them unless specifically instructed otherwise.

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
        parameterParameterValue :: Bool :|: Int :|: Text,
        parameterParameterName :: Text
      }

    data TopLevel = TopLevel {
        topLevelParameter :: Parameter
      }
```

Real-world use case examples are provided in the package [source repository](https://github.com/mgajda/json-autotype/tree/master/test).

Other approaches:
=================

* There was a [json-sampler](https://maxs.io/generating-types-from-json-samples/) that allows to make simpler data structure from JSON examples, but doesn't seem to perform unification, nor is it suitable for big APIs.

* There is a [TypeScript type provider](https://jvilk.com/MakeTypes/), and [PLDI 2016 paper](https://dl.acm.org/citation.cfm?id=2908115) on solving this problem using <em>preferred type shapes</em> instead of union types.
One can think about it as a alternative theory that gives very similar results, with more complicated exposition. It also does not tackle the problem of tagged records. It also does not attempt to <em>guess</em> unification candidates in order to reduce type complexity.
* [PADS project](https://www.cs.princeton.edu/~dpw/papers/padsml06.pdf) is another attempt to automatically infer types to treat <em>arbitrary</em> data formats (not just JSON). It mixes type declarations, with parsing/printing information in order to have a consistent view of both. It does not handle automatic type inference though.
* [JSON Schema generator](https://www.newtonsoft.com/jsonschema/help/html/GenerateSchema.htm) uses .NET types to generate JSON Schema instead (in opposite direction.) Similar schema generation is [used here](https://sixgun.wordpress.com/2012/02/09/using-json-net-to-generate-jsonschema/)
* Microsoft advocates use of [Data Contracts](https://docs.microsoft.com/en-us/dotnet/framework/wcf/feature-details/using-data-contracts) instead to constrain possible input data.
