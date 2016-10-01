#!/bin/bash

stack clean
rm -rf java/classes/*.class

bin/makejar.sh
# ghc Test.hs
stack build
