#!/usr/bin/python

## Locate library
import sys
sys.path.append("stokeasm_py")

from stokeasm import att_exec

## Test

teststr = "xorq %eax, %rax\n"
teststr = teststr + "retq\n"
print att_exec(teststr)

