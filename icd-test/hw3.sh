#!/bin/sh

stack build nctumpc:icd-hw3-test-exe
cp "$(stack path --dist-dir)/build/icd-hw3-test-exe/icd-hw3-test-exe" \
    icd-test-data/hw3/parser

make clean test -C icd-test-data/hw3/test

rm icd-test-data/hw3/parser
