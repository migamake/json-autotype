FEATURES TO ADD IN THE FUTURE:
==============================

1. Consider rolling out sum types instead of layers of Eithers.
2. Use own FromJSON/ToJSON to allow for the same key name to serve with different types in different structures (e.g. typename prefix to the key.)
3. Allow use as a module, with a parameter recognizing substructures of the special types (like date encoding), and thus generate them.
4. Parse multiple JSON files into a unified type declaration.
