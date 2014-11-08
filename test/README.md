Classification of unit tests:
=============================

I classified unit tests into 3 filetypes, even though they all contain sample JSON content:

- .json files are valid JSON that is treated by AutoType in error-free way.
- .bad_json files are invalid JSON according to Aeson parser "decode" method.
- .auto_error are JSON files valid according to Aeson, but for which AutoType produces incorrect code.
- .explicit_parser are JSON files valid according to Aeson, but for which AutoType will produce the correct code only after there is explicit distinction between field name, and parsed field name (due to clash with a keyword.)
- .renaming are JSON files that will be valid when explicit parser generation is introduced (as explained above) *and* renaming of clashing fields in different types is *re-introduced* in code.
