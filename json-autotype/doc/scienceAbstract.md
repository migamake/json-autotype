Automatic typing JSON
=====================

I want to tell a story about practical web-service development using type theory.

JSON is a subset of JavaScript, and convenient data format for Web 2.0
applications. Structured Web APIs often use JSON messages to transmit
large answer sets that may be leveraged when building both mash-ups,
and complex web services.

Since Haskell excels in precise description of such complex systems,
it enjoys relatively large set of parsing, validation, and web libraries.

JSON-Autotype facilitates automatic creation of Web API interfaces
in Haskell, and uses union typing for ad-hoc type discovery.

I plan to introduce problem by examples, and discuss most frequent problems
of type-based interface ,,discovery''.
