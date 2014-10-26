/*
Copyright 2013 eric schkufza

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

#include <iostream>

#include "include/x64asm.h"

using namespace std;
using namespace x64asm;

// This example demonstrates how the assembler api can be used to create
// functions which call other functions.

// Prints hello world
void hello() {
  cout << "Hello, world!" << endl;
}

int main() {
  // Create an assembler
  Assembler assm;

  // Example 1:
  // Compile a function that calls a c-function.
  Code c1 {
    {MOV_R64_IMM64, {rax, Imm64{hello}}},
    {CALL_R64, {rax}},
    {RET}
  };
  const auto f1 = assm.assemble(c1);

  // Calling this function should invoke hello()
  f1.call<void>();
  cout << endl;

  // Example 2:
  // Compile a function that calls a previously assembled function.
  Code c2 {
    {MOV_R64_IMM64, {rax, Imm64{f1}}},
    {CALL_R64, {rax}},
    {RET}
  };
  const auto f2 = assm.assemble(c2);

  // Calling this function will indirectly invoke hello()
  f2.call<void>();
  cout << endl;

  // Example 3:
  // Writing a recursive function (fibonacci)
  Code c3 {
    {LABEL_DEFN, {Label{"fib"}}},

    {CMP_R64_IMM32, {rdi, Imm32{0}}},
    {JE_LABEL, {Label{"base_case"}}},
    {CMP_R64_IMM32, {rdi, Imm32{1}}},
    {JE_LABEL, {Label{"base_case"}}},

    {PUSH_R64, {rdi}},
    {SUB_R64_IMM32, {rdi, Imm32{1}}},
    {CALL_LABEL, {Label{"fib"}}},
    {POP_R64, {rdi}},
    {ADD_R64_R64, {rax, rdi}},
    {RET},

    {LABEL_DEFN, {Label{"base_case"}}},
    {MOV_R64_IMM64, {rax, Imm64{1}}},
    {RET}
  };
  const auto f3 = assm.assemble(c3);

  // Calling this function should compute fibbonaci
  cout << "fib(5) = " << f3.call<int, int>(5) << endl;
  cout << endl;

  // Example 4:
  // In princple, it is possible to assemble a self referential function.
  // However if during the course of assembling, the internal buffer associated
  // with f4 is exhausted and the assembler is forced to allocate a new one,
  // this may fail.  Use with caution.
  Function f4;
  Code c4 {
    {CMP_R64_IMM32, {rdi, Imm32{0}}},
    {JE_LABEL, {Label{"base_case"}}},
    {CMP_R64_IMM32, {rdi, Imm32{1}}},
    {JE_LABEL, {Label{"base_case"}}},

    {PUSH_R64, {rdi}},
    {SUB_R64_IMM32, {rdi, Imm32{1}}},
    {MOV_R64_IMM64, {rdx, Imm64{f4}}},
    {CALL_R64, {rdx}},
    {POP_R64, {rdi}},
    {ADD_R64_R64, {rax, rdi}},
    {RET},

    {LABEL_DEFN, {Label{"base_case"}}},
    {MOV_R64_IMM64, {rax, Imm64{1}}},
    {RET}
  };
  assm.assemble(f4, c4);

  // Calling this function should compute fibbonaci as well
  cout << "fib(5) = " << f4.call<int, int>(5) << endl;
  cout << endl;

  return 0;
}

