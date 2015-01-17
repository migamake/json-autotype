FEATURES TO ADD IN THE FUTURE:
==============================

1. Allow use as a module, with a parameter recognizing substructures of the special types (like date encoding), and thus generate them.
2. Export to other languages:
    * JSON Schema (WAML?)
        - http://json-schema.org/
    * OCaml code
    * Java
3. Try to validate/reconstruct schemas of the documents in JSON database like MongoDB.
4. Add plugins for user type detection:
    * Date/datetimes
    * URLs
    * int vs double
5. Subtyping relation
6. Algebraic sum encoding:
    * transcriptic.com example ("op")
7. Try integration:
    * as GHC preprocessor
    * with Cabal build system.
8. Submodules: allow addressing different files as submodules of the other files.
9. Giving parameters as config file, instead of CLI options.

TESTING:
========
1. Check if fix-point of parse/write/infer types.
