FEATURES TO ADD IN THE FUTURE:
==============================

1. Allow use as a module, with a parameter recognizing substructures of the special types (like date encoding), and thus generate them.
2. Export to other languages:
    * JSON Schema (WAML?)
        - http://json-schema.org/
    * OCaml code
    * Java
3. Import type from JSON schema?
4. Try to validate/reconstruct schemas of the documents in JSON database like MongoDB.
5. Add plugins for user type detection:
    * Date/datetimes
    * URLs
    * int vs double
6. Subtyping relation
7. Algebraic sum encoding:
    * transcriptic.com example ("op")
8. Try integration:
    * as GHC preprocessor
    * with Cabal build system.
9. Submodules: allow addressing different files as submodules of the other files.
10. Giving parameters as config file, instead of CLI options.

TESTING:
========
1. Check if fix-point of parse/write/infer types.
