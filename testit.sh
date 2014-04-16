#!/bin/bash

ghc  Test.hs && ./Test > JSONTypes.hs && ghc JSONTypes.hs
