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

#ifndef X64ASM_SRC_OPCODE_H
#define X64ASM_SRC_OPCODE_H

#include <string>


namespace x64asm {

/** An instruction mnemonic. */
enum Opcode : int32_t {
  // Internal mnemonics
  LABEL_DEFN = 0
  // Auto-generated mnemonics
  #include "src/opcode.enum"
  // Max number of opcodes
  , NUM_OPCODES
};

std::string opcode_write_att(Opcode o);
std::string opcode_write_intel(Opcode o);
std::ostream& opcode_write_text(std::ostream& os, const x64asm::Opcode&);
std::istream& opcode_read_text(std::istream& is, x64asm::Opcode&);

/** Returns the same opcode with any 32-bit label jumps changed to 8-bit ones. */
Opcode label32_transform(Opcode c);

} // namespace x64asm

namespace std {

/** ostream overload. */
inline ostream& operator<<(ostream& os, const x64asm::Opcode& op) {
  return opcode_write_text(os, op);
}

/** ostream overload. */
inline istream& operator>>(istream& is, x64asm::Opcode& op) {
  return opcode_read_text(is, op);
}

} // namespace std


#define X64ASM_NUM_OPCODES x64asm::Opcode::NUM_OPCODES

#endif
