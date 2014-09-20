Classification of unit tests:
=============================

I classified unit tests into 3 filetypes, even though they all contain sample JSON content:
a) .json files are valid JSON that is treated by AutoType in error-free way.
b) .bad_json files are invalid JSON according to Aeson parser "decode" method.
c) .auto_error are JSON files valid according to Aeson, but for which AutoType produces incorrect code.
