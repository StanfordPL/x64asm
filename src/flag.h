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

#ifndef X64ASM_SRC_FLAG_H
#define X64ASM_SRC_FLAG_H

#include <iostream>
#include <stdint.h>

namespace x64asm {

/** CPUID Feature Flags. */
enum class Flag : uint64_t {
  FPU =       0x0000000000000001, // On-board FPU
  TSC =       0x0000000000000002, // RDTSC
  MSR =       0x0000000000000004, // RDMSR WRMSR
  CX8 =       0x0000000000000008, // CMPXCHG8
  SEP =       0x0000000000000010, // SYSENTER SYSEXIT
  CMOV =      0x0000000000000020, // CMOV FCMOV
  CLFLUSH =   0x0000000000000040, // CLFLUSH
  MMX =       0x0000000000000080, // Multimedia Extensions
  FXSR =      0x0000000000000100, // FXSAVE FXRSTOR
  SSE =       0x0000000000000200, // Intel SSE Vector Instructions
  SSE2 =      0x0000000000000400, // SSE2
  SYSCALL =   0x0000000000000800, // SYSCALL SYSRET
  RDTSCP =    0x0000000000001000, // RDTSCP
  REP_GOOD =  0x0000000000002000, // REP Microcode works well
  NOPL =      0x0000000000004000, // NOPL (0f 1f) Instructions
  PNI =       0x0000000000008000, // SSE3 (Prescott New Instructions)
  PCLMULQDQ = 0x0000000000010000, // PCLMULQDQ
  MONITOR =   0x0000000000020000, // MONITOR MWAIT
  SSSE3 =     0x0000000000040000, // Supplemental SSE-3
  FMA =       0x0000000000080000, // Fused Multiply-Add
  CX16 =      0x0000000000100000, // CMPXCHG16B
  SSE4_1 =    0x0000000000200000, // SSE-4.1
  SSE4_2 =    0x0000000000400000, // SSE-4.2
  MOVBE =     0x0000000000800000, // MOVBE
  POPCNT =    0x0000000001000000, // POPCNT
  AES =       0x0000000002000000, // AES instructions
  XSAVE =     0x0000000004000000, // XSAVE XRSTOR XSETBV XGETBV
  AVX =       0x0000000008000000, // Advanced Vector Extensions
  F16C =      0x0000000010000000, // 16-bit FP Conversions
  RDRAND =    0x0000000020000000, // RDRAND
  LAHF_LM =   0x0000000040000000, // LAHF SAHF (in long mode)
  ABM =       0x0000000080000000, // Advanced Bit Manipulation
  XSAVEOPT =  0x0000000100000000, // Optimized XSave
  FSGSBASE =  0x0000000200000000, // {RD/WR}{FS/GS}BASE
  BMI1 =      0x0000000400000000, // 1st Group Bit-manipulation Extensions
  HLE =       0x0000000800000000, // Hardware Lock Ellision
  AVX2 =      0x0000001000000000, // AVX2 Instructions
  BMI2 =      0x0000002000000000, // 2nd Group Bit-manipulation Extensions
  ERMS =      0x0000004000000000, // Enhanced REP MOVSB/STOSB
  INVPCID =   0x0000008000000000, // Invalidate Processor Context ID
  RTM =       0x0000010000000000  // Restricted Transactional Memory
};

/** Reads a flag from an istream in text. */
std::istream& read_text(std::istream& is, Flag& f);
/** Writes a flag to an ostream using text. */
std::ostream& write_text(std::ostream& os, const Flag f);

} // namespace x64asm

namespace std {

/** iostream overload */
inline istream& operator>>(istream& is, x64asm::Flag& f) {
  return read_text(is, f);
}
/** iostream overload */
inline ostream& operator<<(ostream& os, const x64asm::Flag f) {
  return write_text(os, f);
}

} // namespace std

#endif
