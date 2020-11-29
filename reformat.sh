#!/bin/bash
set -v -e
for i in src/*.res* __tests__/*.res; do
  ./node_modules/.bin/bsc -format $i > $i.out
  mv $i.out $i
done
