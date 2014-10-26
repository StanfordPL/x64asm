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

// This example demonstrates how the assembler api interacts with the
// linux abi. This example will not work on a machine which uses the
// windows abi.

int main() {
  // Create an assembler
  Assembler assm;

  // Compile a function with two distinct code paths.
  Code c1 {
    // 1. Round up the floating point value in %xmm0.
    {ROUNDSS_XMM_XMM_IMM8, {xmm0, xmm0, Imm8{2}}},
    // 2. Add ten to the integer value in %rdi and place the result in %rax.
    {ADD_R64_IMM32, {rdi, Imm32{10}}},
    {MOV_R64_R64, {rax, rdi}},
    // The semantics of this return statement depend on usage.
    {RET}
  };
  const auto f1 = assm.assemble(c1);

  // Calling the function as one that takes and returns a floating point value
  // will make use of the first path. The linux abi will return the floating
  // point value in %xmm0.
  cout << "fxn(2.2) = " << f1.call<float, float>(2.2) << endl;

  // Calling the function as one that takes and returns an integer value
  // will make use of the second path. The linux abi will return the integer
  // value in %rax.
  cout << "fxn(2) = " << f1.call<int, int>(2) << endl;

  return 0;
}


