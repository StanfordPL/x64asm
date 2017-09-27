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

// This example demonstrates how the assembler api can be used to pass
// program state back and forth between assembled functions.

int main() {
  // Create an assembler
  Assembler assm;

  // Declare a local program variable.
  uint64_t x = 10;

  // Example 1:
  // Compile a function that will modify the value of x as a side effect
  // Note that x is referenced by its address.
  Code c1 {
    {MOV_R64_IMM64, {rax, Imm64{&x}}},
    {INC_M64, {M64{rax}}},
    {RET}
  };
  const auto f1 = assm.assemble(c1).second;

  // Calling the function should increment the value of x
  cout << "Before: x = " << x << endl;
  f1.call<void>();
  cout << "After:  x = " << x << endl;
  cout << endl;

  // Example 2:
  // Compile a function that will copy the value of rdx to x as a side effect
  // Note that rdx only escapes this function through the pointer to x
  Code c2 {
    {MOV_R64_IMM64, {rax, Imm64{&x}}},
    {MOV_R64_IMM64, {rdx, Imm64{12345678}}},
    {MOV_M64_R64, {M64{rax}, rdx}},
    {RET}
  };
  const auto f2 = assm.assemble(c2).second;

  // Calling the function should place the value of rdx in x
  cout << "Before: x = " << x << endl;
  f2.call<void>();
  cout << "After:  x = " << x << endl;
  cout << endl;

  return 0;
}

