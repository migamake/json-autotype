#!/bin/bash

source ci/common.sh

message "Stack setup"
stack --version
stack setup --system-ghc

message "Stack build"
stack build

message "Stack test"
stack test --verbose

