FEATURES TO ADD IN THE FUTURE:
==============================

1. Algebraic sum encoding:
    * transcriptic.com example ("op")
    * JSON schema example ("type")
2. Allow use as a module, with a parameter recognizing substructures of the special types (like date encoding), and thus generate them.
3. Export to other languages:
    * JSON Schema (WAML?)
        - http://json-schema.org/
	- https://hackage.haskell.org/package/json-schema-0.7.4.0/docs/Data-JSON-Schema-Types.html#t:Schema
    * OCaml code
    * Java
    * Swift?
    * Emacs Lisp? https://github.com/SavchenkoValeriy/jeison
4. Import type from JSON schema.
5. Add plugins for user type detection:
    * Date/datetimes
    * URLs
    * int vs double
6. Subtyping relation

Possible features that do not seem a priority:
==============================================
1. Try integration:
    * as GHC preprocessor
    * with Cabal build system.
2. Submodules: allow addressing different files as submodules of the other files.
3. Giving parameters as config file, instead of CLI options.
4. Check that module name is valid Haskell module name and show appropriate error message, possibly treat it to make a good module name.
5. Add conversion of a given path to Map String v
6. Try to validate/reconstruct schemas of the documents in JSON database like MongoDB.

TESTING:
========
1. Check if fix-point of parse/write/infer types.
2. Check writing with `toEncoding`.
