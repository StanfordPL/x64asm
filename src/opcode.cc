/*
Copyright 2013-2015 Stanford University

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

#include <array>

#include "src/opcode.h"

using namespace std;
using namespace x64asm;

namespace {

constexpr array<const char*, X64ASM_NUM_OPCODES> att_() {
  return {
    // Internal mnemonics
    "<label>"
    // Auto-generated mnemonics
    #include "src/opcode.names"
  };
}

} // namespace

namespace x64asm {

/** Get the string representation of an opcode */
string opcode_write_att(Opcode o) {
  return string(att_()[o]);
}

/** Read the string representation of an opcode */
istream& opcode_read_att(istream& is, x64asm::Opcode& op) {
  string s;
  is >> s;
  for(size_t i = 0; i < X64ASM_NUM_OPCODES; ++i) {
    if(string(att_()[i]) == s) {
      op = (Opcode)i;
      break;
    }
  }
  return is;
}

} // namespace x64asm



