#!/bin/bash

mv $1 $1.s
gcc -c $1.s -o $1.o
objdump -d $1.o  | tail -n+8 | cut -c11- > $1.dump
./run.py $1 > /dev/null 2> /dev/null
