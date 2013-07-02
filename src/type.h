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

#ifndef X64ASM_SRC_TYPE_H
#define X64ASM_SRC_TYPE_H

namespace x64asm {

/** Any of the types which appear in the Intel Manual. */
enum class Type {
  HINT = 0,

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

  RL,
  RH,
  RB,
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

  YMM,

	ZMM
};

} // namespace x64asm

#endif
