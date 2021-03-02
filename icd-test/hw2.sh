#!/bin/sh

stack build nctumpc:icd-hw2-test-exe
cp "$(stack path --dist-dir)/build/icd-hw2-test-exe/icd-hw2-test-exe" \
    icd-test-data/hw2/parser

make clean test -C icd-test-data/hw2/test

rm icd-test-data/hw2/parser
