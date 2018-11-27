Min description:

1. Type plugin:
  * recognizer:
    - recognize :: exists a. Value -> Maybe (Type a)
  * type name
    - class Types where name :: a -> String
  * type "type" (usually constant constructor?)
    - instance of Types
2. Type domain:
  * < relations (or better :< or <:)

---------------------------------------------------------------------------------
Seems that I need lattice defined by PoSet:

* Poset packages:
  - https://hackage.haskell.org/package/altfloat-0.3.1/docs/Data-Poset.html

* Lattice packages:
  - http://hackage.haskell.org/package/lattices-1.2.1.1/docs/Algebra-Lattice.html
  - http://hackage.haskell.org/package/Lattices-0.0.1/docs/Math-Lattices-LLL.html
