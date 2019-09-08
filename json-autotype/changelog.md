Changelog
=========
    3.0.2  Sep 2019
        * Relax `lens`, `hashable` version bounds for GHC 8.8.1 update.

    3.0.0  Nov 2018
        * Distinguishing integers and floats.
        * Hide all API beside Alternative (as unused outside generator).
        * Add fixity for alt (#20)
        * Use `eitherDecode` instead of `decode` to get better error messages.
        * Split Data.Aeson.AutoType.Alternative to `json-alt`.

    2.0.2  Nov 2018
        * Clean up the tests.
        * Remove compatibility with Aeson versions earlier than 1.2.1.
        * Removed all CPP macros
        * Add --version

    2.0.1  Nov 2018
        * Better error reporting when parsing JSON.

    2.0.0  Jun 2018
        * Elm support completed with untagged unions
        * Add HaskellStrict option for running tests with -Werror, -Wall by default.
        * Make random tests run with -Werror, -Wall by default
        * Update dependencies to Aeson ranged between 1.2.1-1.4

    1.1.2  Mar 2018
        * Fixed maintainer list.

    1.1.1  Mar 2018
        * Fixed test builds (for Haskell).

    1.1.0  Mar 2018
        * Partial support Elm code generation.

    1.0.19  Nov 2017
        * Allow to have a custom name for toplevel data type.

    1.0.18  Nov 2017
        * Fixed unit tests.
        * Fixed import for inclusion in Stackage.

    1.0.17  Nov 2017
        * Fixed build and test issues.

    1.0.16  Nov 2017
        * Dependencies updated to resolve #12, #15.
        * Fixed orphan Generic for Aeson >= 1.2.1 (#14).
        * Cleaned option parsing code.
        * Qualify GHC.Generics import.
        * Switch to optparse-applicative
        * Option to explicitly unify selected entries

    1.0.15  Dec 2016
        * Support YAML input.

    1.0.14  May 2016
        * Update to latest lens.

    1.0.13  Mar 2016
        * Bumped up hint upper bound for v0.5.

    1.0.12  Mar 2016

        * Fixed issue #8 - misrepresenting Double as Int.
        * Fixed issue #9 - efficient formatting with new Aeson-0.10 builder (toEncoding.)

    1.0.11  Mar 2016

        * Updated to GHC 8.0

    1.0.10  Sep 2015

        * Fixed bug appeared with aeson 0.10 breaking change:
	https://github.com/bos/aeson/issues/287

    1.0.8  Sep 2015

        * Dependency bump for lens 4.13 and aeson 0.10.

    1.0.7  Jul 2015

        * Dependency bump for lens and vector.

    1.0.6  Jun 2015

        * Make lens and aeson versions consistent in the *.cabal file.

    1.0.3-1.0.5  Jun 2015

        * Bumped Aeson dependency up.
        * Tiny docs corrections.

    1.0.2  Jun 2015

        * Relaxed dependency for lens-4.11.

    1.0.1  Apr 2015

        * Relaxed dependency to lens-4.10.

    1.0  Apr 2015

        * First stable release.

    0.5  Apr 2015

        * Reduced name space pollution when generating code.
          Now all valid JSON test examples do work.
        * Corrected build failure on GHC 7.8.4

    0.4  Apr 2015

        * Release candidate for current functionality.

    0.3  Apr 2015

        * Passed all smallcheck/quickcheck tests.
        * Approaching release candidate.

    0.2.5.13  Apr 2015

        * Correctly handling lone option, not yet union with optionality.
          Fixed: #3.

    0.2.5.12  Apr 2015

        * Added typechecking before and after type unification.
        * Added shrink for more informative QuickCheck testing.
        * Tested mostly using GHC 7.10.

    0.2.5.11  Mar 2015

        * Add short versions of command line flags: -o, -d, and -t.

    0.2.5.10  Mar 2015

        * Bump up lens dependency.

    0.2.5.8  Mar 2015

        * Updated tests and build config.

    0.2.5.7  Mar 2015

        * Fixed documentation anchors, and unit test classification for failures.

    0.2.5.6  Mar 2015

        * Relaxed upper bounds for lens 4.8.

    0.2.5.5  Mar 2015

        * (Skipped this version number by mistake.)

    0.2.5.4  Dec 2014

        * Relaxed upper bounds for new lens.

    0.2.5.3  Dec 2014

        * Relaxed upper bounds again.

    0.2.5.2  Dec 2014

        * Updated metainfo, relaxed upper bounds for GHC 7.10.

    0.2.5.0  Nov 2014

        * Nicer union type syntax in Data.Aeson.AutoType.Alternative.

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
