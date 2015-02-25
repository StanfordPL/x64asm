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

#ifndef X64ASM_SRC_OPCODE_H
#define X64ASM_SRC_OPCODE_H

#include <string>


namespace x64asm {

#define X64ASM_NUM_OPCODES 3819

/** An instruction mnemonic. */
enum Opcode : int32_t {
  // Internal mnemonics
  LABEL_DEFN = 0
               // Auto-generated mnemonics
#include "src/opcode.enum"
};

std::string opcode_write_att(Opcode o);

} // namespace x64asm

#endif
