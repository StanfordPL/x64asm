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

#ifndef X64ASM_SRC_TYPE_H
#define X64ASM_SRC_TYPE_H

#include <cassert>
#include <sstream>
#include <iostream>
#include <stdint.h>

namespace x64asm {

/** Any of the types which appear in the Intel Manual. */
enum class Type {
  NONE = 0, //for an operand constructed without a type

  HINT,

  IMM_8,
  IMM_16,
  IMM_32,
  IMM_64,
  ZERO,
  ONE,
  THREE,

  LABEL,

  M_8,
  M_16,
  M_32,
  M_64,
  M_128,
  M_256,
  M_16_INT,
  M_32_INT,
  M_64_INT,
  M_32_FP,
  M_64_FP,
  M_80_FP,
  M_80_BCD,
  M_2_BYTE,
  M_28_BYTE,
  M_108_BYTE,
  M_512_BYTE,
  FAR_PTR_16_16,
  FAR_PTR_16_32,
  FAR_PTR_16_64,

  MM,

  PREF_66,
  PREF_REX_W,
  FAR,

  MOFFS_8,
  MOFFS_16,
  MOFFS_32,
  MOFFS_64,

  R_8,
  RH,
  AL,
  CL,
  R_16,
  AX,
  DX,
  R_32,
  EAX,
  R_64,
  RAX,

  REL_8,
  REL_32,

  SREG,
  FS,
  GS,

  ST,
  ST_0,

  XMM,
  XMM_0,

  YMM
};

uint16_t bit_width_of_type(Type t);
bool is_type_gp_register(Type t);
bool is_type_sse_register(Type t);
bool is_type_mm_register(Type t);
bool is_type_typical_memory(Type t);
bool is_type_immediate(Type t);

} // namespace x64asm

namespace std {

/** ostream overload. */
inline ostream& operator<<(ostream& os, const x64asm::Type& t) {
  switch (t) {
  case x64asm::Type::IMM_8: os << "imm8" << endl; break;
  case x64asm::Type::IMM_16: os << "imm16" << endl; break;
  case x64asm::Type::IMM_32: os << "imm32" << endl; break;
  case x64asm::Type::IMM_64: os << "imm64" << endl; break;
  case x64asm::Type::ZERO: os << "zero" << endl; break;
  case x64asm::Type::HINT: os << "hint" << endl; break;
  case x64asm::Type::ONE: os << "one" << endl; break;
  case x64asm::Type::THREE: os << "three" << endl; break;
  case x64asm::Type::LABEL: os << "label" << endl; break;
  case x64asm::Type::M_8: os << "m8" << endl; break;
  case x64asm::Type::M_16: os << "m16" << endl; break;
  case x64asm::Type::M_32: os << "m32" << endl; break;
  case x64asm::Type::M_64: os << "m64" << endl; break;
  case x64asm::Type::M_128: os << "m128" << endl; break;
  case x64asm::Type::M_256: os << "m256" << endl; break;
  case x64asm::Type::M_16_INT: os << "m16int" << endl; break;
  case x64asm::Type::M_32_INT: os << "m32int" << endl; break;
  case x64asm::Type::M_64_INT: os << "m64int" << endl; break;
  case x64asm::Type::M_32_FP: os << "m32fp" << endl; break;
  case x64asm::Type::M_64_FP: os << "m64fp" << endl; break;
  case x64asm::Type::M_80_FP: os << "m80fp" << endl; break;
  case x64asm::Type::M_80_BCD: os << "m80bcd" << endl; break;
  case x64asm::Type::M_2_BYTE: os << "m2byte" << endl; break;
  case x64asm::Type::M_28_BYTE: os << "m28byte" << endl; break;
  case x64asm::Type::M_108_BYTE: os << "m108byte" << endl; break;
  case x64asm::Type::M_512_BYTE: os << "m512byte" << endl; break;
  case x64asm::Type::FAR_PTR_16_16: os << "farptr1616" << endl; break;
  case x64asm::Type::FAR_PTR_16_32: os << "farptr1632" << endl; break;
  case x64asm::Type::FAR_PTR_16_64: os << "farptr1664" << endl; break;
  case x64asm::Type::MM: os << "mm" << endl; break;
  case x64asm::Type::MOFFS_8: os << "moffs8" << endl; break;
  case x64asm::Type::MOFFS_16: os << "moffs16" << endl; break;
  case x64asm::Type::MOFFS_32: os << "moffs32" << endl; break;
  case x64asm::Type::MOFFS_64: os << "moffs_64" << endl; break;
  case x64asm::Type::PREF_66: os << "pref66" << endl; break;
  case x64asm::Type::PREF_REX_W: os << "prefrexw" << endl; break;
  case x64asm::Type::FAR: os << "far" << endl; break;
  case x64asm::Type::RH: os << "rh" << endl; break;
  case x64asm::Type::AL: os << "al" << endl; break;
  case x64asm::Type::CL: os << "cl" << endl; break;
  case x64asm::Type::R_8: os << "r8" << endl; break;
  case x64asm::Type::AX: os << "ax" << endl; break;
  case x64asm::Type::DX: os << "dx" << endl; break;
  case x64asm::Type::R_16: os << "r16" << endl; break;
  case x64asm::Type::EAX: os << "eax" << endl; break;
  case x64asm::Type::R_32: os << "r32" << endl; break;
  case x64asm::Type::RAX: os << "rax" << endl; break;
  case x64asm::Type::R_64: os << "r64" << endl; break;
  case x64asm::Type::REL_8: os << "rel8" << endl; break;
  case x64asm::Type::REL_32: os << "rel32" << endl; break;
  case x64asm::Type::FS: os << "fs" << endl; break;
  case x64asm::Type::GS: os << "gs" << endl; break;
  case x64asm::Type::SREG: os << "sreg" << endl; break;
  case x64asm::Type::ST_0: os << "st0" << endl; break;
  case x64asm::Type::ST: os << "st" << endl; break;
  case x64asm::Type::XMM_0: os << "xmm0" << endl; break;
  case x64asm::Type::XMM: os << "xmm" << endl; break;
  case x64asm::Type::YMM: os << "ymm" << endl; break;
  default:
    assert(false);
  }
  return os;
}

} // namespace std

#endif
