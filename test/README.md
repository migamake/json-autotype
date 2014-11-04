Classification of unit tests:
=============================

I classified unit tests into 3 filetypes, even though they all contain sample JSON content:

- .json files are valid JSON that is treated by AutoType in error-free way.
- .bad_json files are invalid JSON according to Aeson parser "decode" method.
- .auto_error are JSON files valid according to Aeson, but for which AutoType produces incorrect code.
