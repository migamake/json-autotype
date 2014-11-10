Changelog
=========
    0.2.4.0  Nov 2014

        * To assure proper treatment of unions,
          I make them with Data.Aeson.AutoType.Alternative type instead of Either.

    0.2.3.0  Nov 2014

        * Explicit JSON parser generation to avoid conflicts between Haskell keywords and field names.
        * Renaming of Haskell field names with a prefix of object name (data type.)

    0.2.2.0  Nov 2014

        * GenerateJSONParser may now take multiple input samples to produce single parser.
        * Fixed automated testing for all example files.

    0.2.1.4  Oct 2014

        * Added examples to the package distribution.

    0.2.1.3  Oct 2014

        * Cleaned up package.
        * Changelog in markdown format.

    0.2.1  Oct 2014

        * Added option to use it as a filter ('-' is accepted input name.)

    0.2.0  Oct 2014

        * First release to Hackage.
        * Handling of proper unions, and most examples.
        * Automatically tested on a wide range of example documents (see
        tests/)
        * Initial documentation in README.md.

    0.1.0  July 2014

	* First experiments uploaded to GitHub, and discussed to
	HackerSpace.SG.

