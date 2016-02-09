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

#ifndef X64ASM_SRC_TYPE_TRAITS_H
#define X64ASM_SRC_TYPE_TRAITS_H

#include <type_traits>

#include "src/hint.h"
#include "src/imm.h"
#include "src/label.h"
#include "src/m.h"
#include "src/mm.h"
#include "src/modifier.h"
#include "src/moffs.h"
#include "src/r.h"
#include "src/rel.h"
#include "src/sreg.h"
#include "src/st.h"
#include "src/xmm.h"
#include "src/ymm.h"

namespace x64asm {

// Attributes for template meta-programming. This module is for internal
// use only and not exposed as part of the public interface of x64asm.

// Is this a concrete operand type?

template <typename T>
struct is_operand : public std::false_type { };

template <>
struct is_operand<Operand> : public std::true_type { };

template <>
struct is_operand<Hint> : public std::true_type { };

template <>
struct is_operand<Imm> : public std::true_type { };
template <>
struct is_operand<Imm8> : public std::true_type { };
template <>
struct is_operand<Imm16> : public std::true_type { };
template <>
struct is_operand<Imm32> : public std::true_type { };
template <>
struct is_operand<Imm64> : public std::true_type { };
template <>
struct is_operand<Zero> : public std::true_type { };
template <>
struct is_operand<One> : public std::true_type { };
template <>
struct is_operand<Three> : public std::true_type { };

template <>
struct is_operand<Label> : public std::true_type { };

template <>
struct is_operand<Mem> : public std::true_type { };
template <>
struct is_operand<M8> : public std::true_type { };
template <>
struct is_operand<M16> : public std::true_type { };
template <>
struct is_operand<M32> : public std::true_type { };
template <>
struct is_operand<M64> : public std::true_type { };
template <>
struct is_operand<M128> : public std::true_type { };
template <>
struct is_operand<M256> : public std::true_type { };
template <>
struct is_operand<M16Int> : public std::true_type { };
template <>
struct is_operand<M32Int> : public std::true_type { };
template <>
struct is_operand<M64Int> : public std::true_type { };
template <>
struct is_operand<M32Fp> : public std::true_type { };
template <>
struct is_operand<M64Fp> : public std::true_type { };
template <>
struct is_operand<M80Fp> : public std::true_type { };
template <>
struct is_operand<M80Bcd> : public std::true_type { };
template <>
struct is_operand<M2Byte> : public std::true_type { };
template <>
struct is_operand<M28Byte> : public std::true_type { };
template <>
struct is_operand<M108Byte> : public std::true_type { };
template <>
struct is_operand<M512Byte> : public std::true_type { };
template <>
struct is_operand<FarPtr1616> : public std::true_type { };
template <>
struct is_operand<FarPtr1632> : public std::true_type { };
template <>
struct is_operand<FarPtr1664> : public std::true_type { };

template <>
struct is_operand<Mm> : public std::true_type { };

template <>
struct is_operand<Pref66> : public std::true_type { };
template <>
struct is_operand<PrefRexW> : public std::true_type { };
template <>
struct is_operand<Far> : public std::true_type { };

template <>
struct is_operand<Moffs8> : public std::true_type { };
template <>
struct is_operand<Moffs16> : public std::true_type { };
template <>
struct is_operand<Moffs32> : public std::true_type { };
template <>
struct is_operand<Moffs64> : public std::true_type { };

template <>
struct is_operand<R> : public std::true_type { };
template <>
struct is_operand<R8> : public std::true_type { };
template <>
struct is_operand<Rh> : public std::true_type { };
template <>
struct is_operand<Al> : public std::true_type { };
template <>
struct is_operand<Cl> : public std::true_type { };
template <>
struct is_operand<R16> : public std::true_type { };
template <>
struct is_operand<Ax> : public std::true_type { };
template <>
struct is_operand<Dx> : public std::true_type { };
template <>
struct is_operand<R32> : public std::true_type { };
template <>
struct is_operand<Eax> : public std::true_type { };
template <>
struct is_operand<R64> : public std::true_type { };
template <>
struct is_operand<Rax> : public std::true_type { };

template <>
struct is_operand<Rel8> : public std::true_type { };
template <>
struct is_operand<Rel32> : public std::true_type { };

template <>
struct is_operand<Sreg> : public std::true_type { };
template <>
struct is_operand<Fs> : public std::true_type { };
template <>
struct is_operand<Gs> : public std::true_type { };

template <>
struct is_operand<St> : public std::true_type { };
template <>
struct is_operand<St0> : public std::true_type { };

template <>
struct is_operand<Sse> : public std::true_type { };
template <>
struct is_operand<Xmm> : public std::true_type { };
template <>
struct is_operand<Xmm0> : public std::true_type { };

template <>
struct is_operand<Ymm> : public std::true_type { };

// Is this a register type?

template <typename T>
struct is_reg : public std::false_type { };

template <>
struct is_reg<R> : public std::true_type { };
template <>
struct is_reg<R8> : public std::true_type { };
template <>
struct is_reg<Rh> : public std::true_type { };
template <>
struct is_reg<Al> : public std::true_type { };
template <>
struct is_reg<Cl> : public std::true_type { };
template <>
struct is_reg<R16> : public std::true_type { };
template <>
struct is_reg<Ax> : public std::true_type { };
template <>
struct is_reg<Dx> : public std::true_type { };
template <>
struct is_reg<R32> : public std::true_type { };
template <>
struct is_reg<Eax> : public std::true_type { };
template <>
struct is_reg<R64> : public std::true_type { };
template <>
struct is_reg<Rax> : public std::true_type { };

} // namespace x64asm

#endif
