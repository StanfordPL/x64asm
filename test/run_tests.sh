#!/bin/bash

##This file uses enumerate_all to create files necessary
##for testing.

JOBS=16

rm -rf tmp
mkdir tmp

echo "Creating test cases..."
##Compile enumerate_all
ghc -i../src/codegen enumerate_all.hs

##First, generate lots of instructions, and split them up
./enumerate_all ../src/codegen/x64.csv | split -d -l200000 - "tmp/instrs"

echo "Running tests..."
##Process each file individually (map)
find -H tmp -name "instrs*" -print0 | xargs -P$JOBS -0 -n1 -I Z ./process.sh Z

##Concatenate them, and print outputs (reduce)
cat tmp/*.output > output
echo "PASS:"
grep Pass output | cut -c 14- | grep -o "^[0-9]*" | tr '\n' '+' | echo $(sed 's/+$//') | bc
echo "WARN:"
grep Warn output | cut -c 13- | grep -o "^[0-9]*" | tr '\n' '+' | echo $(sed 's/+$//') | bc
echo "FAIL:"
grep Fail output | cut -c 13- | grep -o "^[0-9]*" | tr '\n' '+' | echo $(sed 's/+$//') | bc


