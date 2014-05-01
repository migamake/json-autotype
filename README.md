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
