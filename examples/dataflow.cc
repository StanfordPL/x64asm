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

// This example demonstrates how to use the instruction API to build a
// dataflow analysis.

// This function iterates over a branch-free code and checks that the read
// set for each instruction is a subset of the def set at that code point.
// The boundary condition to the analysis is provided as an input.
// Returns -1 on success, the line number of the error on failure.
int undef_read(const Code& code, const RegSet& boundary) {
  RegSet def_set = boundary;

  for (size_t i = 0, ie = code.size(); i < ie; ++i) {
    const auto& instr = code[i];

    // Compute the read set for this instruction.
    const auto read_set = instr.maybe_read_set();

    // Check that the read set is a subset of the def set at this point
    if ((read_set & def_set) != read_set) {
      return i;
    }

    // Compute the transfer function of this instruction
    def_set |= instr.maybe_write_set();
    def_set -= instr.maybe_undef_set();
  }

  // If control has reached here, no undefined reads can occur.
  return -1;
}

// Perform the undef read check and report results
void run(const Code& code, const RegSet& boundary) {
  cout << "Performing undef read check for:" << endl;
  cout << endl;
  cout << code << endl;
  cout << endl;

  int res = undef_read(code, boundary);
  if (res >= 0) {
    cout << "Check failed on line " << res << endl;
  } else {
    cout << "Check passed" << endl;
  }
}

int main() {
  // Boundary conditions:
  // rax is assumed live-in to both examples
  RegSet boundary = RegSet::empty();
  boundary += rax;

  // Example 1:
  // This function should pass the undef read check.
  Code c1 {
    // write rdi
    {MOV_R64_IMM64, {rdi, Imm64{0}}},
    // read rdi; read and write rax
    {ADD_R64_R64, {rax, rdi}},
    {RET}
  };
  run(c1, boundary);
  cout << endl;

  // Example 2:
  // This function should fail the undef read check.
  Code c2 {
    // write rdi
    {MOV_R64_IMM64, {rdi, Imm64{0}}},
    // read rdi; read and write rax
    {ADD_R64_R64, {rax, rdi}},
    // undefined read from r8; read and write rax
    {ADD_R64_R64, {rax, r8}},
    {RET}
  };
  run(c2, boundary);
  cout << endl;

  return 0;
}

