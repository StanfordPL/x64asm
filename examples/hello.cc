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

#include <fstream>
#include <iostream>

#include "include/x64asm.h"

using namespace std;
using namespace x64asm;

// This example demonstrates three different methods of assembling and
// invoking a toy implementation of the memcpy function in this directory,
// memcpy.s

// Example 1: Read from file
Function from_file() {
  // Create an assembler.
  Assembler assm;

  // Create a code object and read its contents using the >> operator.
  // In addition to fstreams, this works for all istream types.
  Code c;
  ifstream ifs("memcpy.s");
  ifs >> c;

  // Assemble the code and return the result.
  return assm.assemble(c).second;
}

// Example 2: Write code using in-memory RTL.
Function from_code() {
  // Create an assembler.
  Assembler assm;

  // Create code using the in-memory RTL.
  // In addition to the initializer list constructor method shown below,
  // the Code class supports all all STL sequence container operations
  // (ie: resize, find, clear...).
  Code c {
    {XOR_R64_R64, {rcx, rcx}},
    {LABEL_DEFN, {Label{"loop"}}},
    {CMP_R64_R64, {rcx, rdx}},
    {JE_LABEL, {Label{"done"}}},
    {MOV_R8_M8, {al, M8{rsi, rcx, Scale::TIMES_1}}},
    {MOV_M8_R8, {M8{rdi, rcx, Scale::TIMES_1}, al}},
    {INC_R64, {rcx}},
    {JMP_LABEL, {Label{"loop"}}},
    {LABEL_DEFN, {Label{"done"}}},
    {RET}
  };

  // Assemble the code and return the result.
  return assm.assemble(c).second;
}

// Example 3: Use the assembler API.
Function from_api() {
  // Create an assembler and a function to compile code to.
  Assembler assm;
  Function memcpy;

  // The assembler is stateful, this method specializes it for a function.
  assm.start(memcpy);

  // Instructions are inserted using type-safe API calls.
  // Note that as above, labels can be referenced before being bound.
  assm.xor_(rcx, rcx);
  assm.bind(Label {"loop"});
  assm.cmp(rcx, rdx);
  assm.je(Label {"done"});

  assm.mov(al, M8 {rsi, rcx, Scale::TIMES_1});
  assm.mov(M8 {rdi, rcx, Scale::TIMES_1}, al);
  assm.inc(rcx);
  assm.jmp(Label {"loop"});

  assm.bind(Label {"done"});
  assm.ret();

  // Finish assembly (ie: patch up label references) and return the result.
  assm.finish();
  return memcpy;
}

// Example: Invokes an assembled version of the memcpy function.
void test(const Function& memcpy) {
  const char* source = "Hello, world!";
  char* target = new char[32];

  // Functions can be passed to ostream objects to view their hex encoding.
  cout << "Hex source: " << endl;
  cout << memcpy << endl;

  // A function can be called with up to six arguments, each of which must
  // be or be castable to a native type. No explicit cast is necessary.
  memcpy.call<void, char*, const char*, int>(target, source, 14);
  cout << "After return target = \"" << target << "\"" << endl;
  cout << endl;

  delete target;
}

int main() {
  Function f1 = from_file();
  test(f1);

  Function f2 = from_code();
  test(f2);

  Function f3 = from_api();
  test(f3);

  return 0;
}
