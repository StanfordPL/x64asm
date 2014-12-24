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

#include <array>

#include "src/opcode.h"

using namespace x64asm;

std::array<const char*, 3803> att_ {{
    // Internal mnemonics
    "<label definition>"
    // Auto-generated mnemonics
    #include "src/opcode.att"
}};

/** Get the string representation of an opcode */
std::string opcode_write_att(Opcode o) {
  return std::string(att_[o]);
}




