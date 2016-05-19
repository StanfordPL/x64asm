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

#include "src/type.h"

#include <sstream>
#include <iostream>

using namespace std;

namespace x64asm {

uint16_t bit_width_of_type(Type t) {
  switch (t) {
  case x64asm::Type::IMM_8: return 8;
  case x64asm::Type::IMM_16: return 16;
  case x64asm::Type::IMM_32: return 32;
  case x64asm::Type::IMM_64: return 64;
  case x64asm::Type::ZERO: return 0;
  case x64asm::Type::HINT: return 0;
  case x64asm::Type::ONE: return 0;
  case x64asm::Type::THREE: return 0;
  case x64asm::Type::LABEL: return 0;
  case x64asm::Type::M_8: return 8;
  case x64asm::Type::M_16: return 16;
  case x64asm::Type::M_32: return 32;
  case x64asm::Type::M_64: return 64;
  case x64asm::Type::M_128: return 128;
  case x64asm::Type::M_256: return 256;
  case x64asm::Type::M_16_INT: return 16;
  case x64asm::Type::M_32_INT: return 32;
  case x64asm::Type::M_64_INT: return 64;
  case x64asm::Type::M_32_FP: return 32;
  case x64asm::Type::M_64_FP: return 64;
  case x64asm::Type::M_80_FP: return 80;
  case x64asm::Type::M_80_BCD: return 80;
  case x64asm::Type::M_2_BYTE: return 2;
  case x64asm::Type::M_28_BYTE: return 28;
  case x64asm::Type::M_108_BYTE: return 108;
  case x64asm::Type::M_512_BYTE: return 512;
  case x64asm::Type::FAR_PTR_16_16: return 0;
  case x64asm::Type::FAR_PTR_16_32: return 0;
  case x64asm::Type::FAR_PTR_16_64: return 0;
  case x64asm::Type::MM: return 64;
  case x64asm::Type::MOFFS_8: return 8;
  case x64asm::Type::MOFFS_16: return 16;
  case x64asm::Type::MOFFS_32: return 32;
  case x64asm::Type::MOFFS_64: return 64;
  case x64asm::Type::PREF_66: return 0;
  case x64asm::Type::PREF_REX_W: return 0;
  case x64asm::Type::FAR: return 0;
  case x64asm::Type::RH: return 8;
  case x64asm::Type::AL: return 8;
  case x64asm::Type::CL: return 8;
  case x64asm::Type::R_8: return 8;
  case x64asm::Type::AX: return 16;
  case x64asm::Type::DX: return 16;
  case x64asm::Type::R_16: return 16;
  case x64asm::Type::EAX: return 32;
  case x64asm::Type::R_32: return 32;
  case x64asm::Type::RAX: return 64;
  case x64asm::Type::R_64: return 64;
  case x64asm::Type::REL_8: return 8;
  case x64asm::Type::REL_32: return 32;
  case x64asm::Type::FS: return 0;
  case x64asm::Type::GS: return 0;
  case x64asm::Type::SREG: return 0;
  case x64asm::Type::ST_0: return 0;
  case x64asm::Type::ST: return 0;
  case x64asm::Type::XMM_0: return 128;
  case x64asm::Type::XMM: return 128;
  case x64asm::Type::YMM: return 256;
  default:
    assert(false);
  }
  return 0;
}

bool is_type_gp_register(Type t) {

  switch(t) {
  case Type::RH:
  case Type::R_8:
  case Type::AL:
  case Type::CL:
  case Type::R_16:
  case Type::AX:
  case Type::DX:
  case Type::R_32:
  case Type::EAX:
  case Type::R_64:
  case Type::RAX:
    return true;

  default:
    return false;
  }
}

bool is_type_sse_register(Type t) {
  return t == Type::XMM || t == Type::YMM ||
         t == Type::XMM_0;
}

bool is_type_mm_register(Type t) {
  return t == Type::MM;
}

bool is_type_typical_memory(Type t) {

  switch(t) {
  case Type::M_8:
  case Type::M_16:
  case Type::M_32:
  case Type::M_64:
  case Type::M_128:
  case Type::M_256:
  case Type::MOFFS_8:
  case Type::MOFFS_16:
  case Type::MOFFS_32:
  case Type::MOFFS_64:
    return true;

  default:
    return false;
  }
}

bool is_type_immediate(Type t) {

  switch(t) {
  case Type::IMM_8:
  case Type::IMM_16:
  case Type::IMM_32:
  case Type::IMM_64:
  case Type::ZERO:
  case Type::ONE:
  case Type::THREE:
    return true;

  default:
    return false;
  }

}

} // namespace x64asm
