#!/bin/sh

stack build nctumpc:icd-hw1-test-exe
cp "$(stack path --dist-dir)/build/icd-hw1-test-exe/icd-hw1-test-exe" \
    icd-test-data/hw1/scanner

make clean test -C icd-test-data/hw1/test

rm icd-test-data/hw1/scanner
