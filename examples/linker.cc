/*
Copyright 2014 eric schkufza

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

// This example demonstrates how the linker api can be used to combine
// functions produced by the assembler.

int main() {
  // Create an assembler and a linker
  Assembler assm;
  Linker lnkr;

  // Compile a function, f(x) = x + 1
  Code c1 {
    {LABEL_DEFN, {Label{"f1"}}},
    {MOV_R64_R64, {rax, rdi}},
    {INC_R64, {rax}},
    {RET}
  };
  auto f1 = assm.assemble(c1);

  // Compile a function that references f1
  Code c2 {
    {LABEL_DEFN, {Label{"f2"}}},
    {XOR_R64_R64, {rdi, rdi}},
    {CALL_LABEL, {Label{"f1"}}},
    {MOV_R64_R64, {rdi, rax}},
    {CALL_LABEL, {Label{"f1"}}},
    {RET}
  };
  auto f2 = assm.assemble(c2);

  // Example 1:
  // Linking just f2 should cause an undefined symbol error
  lnkr.start();
  lnkr.link(f2);
  lnkr.finish();

  if (lnkr.undef_symbol()) {
    cout << "Undefined Symbol Error (" << lnkr.get_undef_symbol() << ")!" << endl;
  }

  // Example 2:
  // Linking f1 twice should cause a multiple definition error
  lnkr.start();
  lnkr.link(f1);
  lnkr.link(f1);
  lnkr.link(f2);
  lnkr.finish();

  if (lnkr.multiple_def()) {
    cout << "Multiple Definition Error (" << lnkr.get_multiple_def() << ")!" << endl;
  }

  // Example 3:
  // Linking the two functions together should work correctly
  lnkr.start();
  lnkr.link(f1);
  lnkr.link(f2);
  lnkr.finish();

  // Calling f2 should return 2
  if (lnkr.good()) {
    cout << "Linking finished successfully!" << endl;
  }
  cout << "f2() = " << f2.call<size_t>() << endl;

  return 0;
}
