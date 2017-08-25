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

#include "src/instruction.h"

#include "src/constants.h"
#include "src/label.h"
#include "src/mm.h"
#include "src/moffs.h"
#include "src/opcode.h"
#include "src/rel.h"
#include "src/st.h"
#include "src/xmm.h"
#include "src/ymm.h"

#include <regex>

using namespace std;

namespace x64asm {

bool Instruction::is_xor_reg_reg() const {
  // we special case for xor of two identical registers.  in that case,
  // the instruction sets the value of the register to zero, and this is a
  // safe operation even if the register was undefined before.
  switch (get_opcode()) {
  case PXOR_MM_MM:
  case PXOR_XMM_XMM:
  case XOR_R8_R8:
  case XOR_RH_RH:
  case XOR_R16_R16:
  case XOR_R32_R32:
  case XOR_R64_R64:
  case XORPD_XMM_XMM:
  case XORPS_XMM_XMM:
    if (get_operand<Operand>(0) == get_operand<Operand>(1)) {
      return true;
    }
    break;

  case VPXOR_XMM_XMM_M128:
  case VPXOR_XMM_XMM_XMM:
  case VPXOR_YMM_YMM_M256:
  case VPXOR_YMM_YMM_YMM:

  case VXORPD_XMM_XMM_M128:
  case VXORPD_XMM_XMM_XMM:
  case VXORPD_YMM_YMM_M256:
  case VXORPD_YMM_YMM_YMM:
  case VXORPS_XMM_XMM_M128:
  case VXORPS_XMM_XMM_XMM:
  case VXORPS_YMM_YMM_M256:
  case VXORPS_YMM_YMM_YMM:

    if (get_operand<Operand>(1) == get_operand<Operand>(2)) {
      return true;
    }
    break;

  default:
    return false;
  }
  return false;
}

RegSet& Instruction::explicit_must_read_set(RegSet& ret) const {
  if (is_xor_reg_reg()) {
    return ret;
  }
  if ((get_opcode() == Opcode::XCHG_EAX_R32 && get_operand<R32>(1) == eax) ||
      (get_opcode() == Opcode::XCHG_R32_EAX && get_operand<R32>(0) == eax)) {
    // xchg with eax hard-coded is essentially a nop
    return ret;
  }
  for (size_t i = 0, ie = arity(); i < ie; ++i) {
    switch (type(i)) {
    case Type::M_8:
    case Type::M_16:
    case Type::M_32:
    case Type::M_64:
    case Type::M_128:
    case Type::M_256:
    case Type::M_16_INT:
    case Type::M_32_INT:
    case Type::M_64_INT:
    case Type::M_32_FP:
    case Type::M_64_FP:
    case Type::M_80_FP:
    case Type::M_80_BCD:
    case Type::M_2_BYTE:
    case Type::M_28_BYTE:
    case Type::M_108_BYTE:
    case Type::M_512_BYTE:
    case Type::FAR_PTR_16_16:
    case Type::FAR_PTR_16_32:
    case Type::FAR_PTR_16_64:
      ret += get_operand<M8>(i);
      continue;

    case Type::MOFFS_8:
    case Type::MOFFS_16:
    case Type::MOFFS_32:
    case Type::MOFFS_64:
      ret += get_operand<Moffs8>(i);
      continue;

    default:
      break;
    }

    if (!must_read(i)) {
      continue;
    }

    switch (type(i)) {
    case Type::MM:
      ret += get_operand<Mm>(i);
      break;
    case Type::RH:
      ret += get_operand<Rh>(i);
      break;
    case Type::AL:
    case Type::CL:
    case Type::R_8:
      ret += get_operand<R8>(i);
      break;
    case Type::AX:
    case Type::DX:
    case Type::R_16:
      ret += get_operand<R16>(i);
      break;
    case Type::EAX:
    case Type::R_32:
      ret += get_operand<R32>(i);
      break;
    case Type::RAX:
    case Type::R_64:
      ret += get_operand<R64>(i);
      break;
    case Type::FS:
    case Type::GS:
    case Type::SREG:
      ret += get_operand<Sreg>(i);
      break;
    case Type::ST_0:
    case Type::ST:
      ret += get_operand<St>(i);
      break;
    case Type::XMM_0:
    case Type::XMM:
      ret += get_operand<Xmm>(i);
      break;
    case Type::YMM:
      ret += get_operand<Ymm>(i);
      break;

    default:
      break;
    }
  }

  return ret;
}

RegSet& Instruction::explicit_maybe_read_set(RegSet& ret) const {
  if (is_xor_reg_reg()) {
    return ret;
  }
  if ((get_opcode() == Opcode::XCHG_EAX_R32 && get_operand<R32>(1) == eax) ||
      (get_opcode() == Opcode::XCHG_R32_EAX && get_operand<R32>(0) == eax)) {
    // xchg with eax hard-coded is essentially a nop
    return ret;
  }
  for (size_t i = 0, ie = arity(); i < ie; ++i) {
    switch (type(i)) {
    case Type::M_8:
    case Type::M_16:
    case Type::M_32:
    case Type::M_64:
    case Type::M_128:
    case Type::M_256:
    case Type::M_16_INT:
    case Type::M_32_INT:
    case Type::M_64_INT:
    case Type::M_32_FP:
    case Type::M_64_FP:
    case Type::M_80_FP:
    case Type::M_80_BCD:
    case Type::M_2_BYTE:
    case Type::M_28_BYTE:
    case Type::M_108_BYTE:
    case Type::M_512_BYTE:
    case Type::FAR_PTR_16_16:
    case Type::FAR_PTR_16_32:
    case Type::FAR_PTR_16_64:
      ret += get_operand<M8>(i);
      continue;

    case Type::MOFFS_8:
    case Type::MOFFS_16:
    case Type::MOFFS_32:
    case Type::MOFFS_64:
      ret += get_operand<Moffs8>(i);
      continue;

    default:
      break;
    }

    if (!maybe_read(i)) {
      continue;
    }

    switch (type(i)) {
    case Type::MM:
      ret += get_operand<Mm>(i);
      break;
    case Type::RH:
      ret += get_operand<Rh>(i);
      break;
    case Type::AL:
    case Type::CL:
    case Type::R_8:
      ret += get_operand<R8>(i);
      break;
    case Type::AX:
    case Type::DX:
    case Type::R_16:
      ret += get_operand<R16>(i);
      break;
    case Type::EAX:
    case Type::R_32:
      ret += get_operand<R32>(i);
      break;
    case Type::RAX:
    case Type::R_64:
      ret += get_operand<R64>(i);
      break;
    case Type::FS:
    case Type::GS:
    case Type::SREG:
      ret += get_operand<Sreg>(i);
      break;
    case Type::ST_0:
    case Type::ST:
      ret += get_operand<St>(i);
      break;
    case Type::XMM_0:
    case Type::XMM:
      ret += get_operand<Xmm>(i);
      break;
    case Type::YMM:
      ret += get_operand<Ymm>(i);
      break;

    default:
      break;
    }
  }

  return ret;
}

RegSet& Instruction::explicit_must_write_set(RegSet& ret) const {
  if ((get_opcode() == Opcode::XCHG_EAX_R32 && get_operand<R32>(1) == eax) ||
      (get_opcode() == Opcode::XCHG_R32_EAX && get_operand<R32>(0) == eax)) {
    // xchg with eax hard-coded is essentially a nop
    return ret;
  }
  for (size_t i = 0, ie = arity(); i < ie; ++i) {
    if (must_extend(i))
      switch (type(i)) {
      case Type::EAX:
      case Type::R_32:
        ret += get_operand<R64>(i);
        break;
      case Type::XMM_0:
      case Type::XMM:
        ret += get_operand<Ymm>(i);
        break;
      default:
        assert(false);
        break;
      }
    else if (must_write(i))
      switch (type(i)) {
      case Type::MM:
        ret += get_operand<Mm>(i);
        break;
      case Type::RH:
        ret += get_operand<Rh>(i);
        break;
      case Type::AL:
      case Type::CL:
      case Type::R_8:
        ret += get_operand<R8>(i);
        break;
      case Type::AX:
      case Type::DX:
      case Type::R_16:
        ret += get_operand<R16>(i);
        break;
      case Type::EAX:
      case Type::R_32:
        ret += get_operand<R32>(i);
        break;
      case Type::RAX:
      case Type::R_64:
        ret += get_operand<R64>(i);
        break;
      case Type::FS:
      case Type::GS:
      case Type::SREG:
        ret += get_operand<Sreg>(i);
        break;
      case Type::ST_0:
      case Type::ST:
        ret += get_operand<St>(i);
        break;
      case Type::XMM_0:
      case Type::XMM:
        ret += get_operand<Xmm>(i);
        break;
      case Type::YMM:
        ret += get_operand<Ymm>(i);
        break;

      default:
        break;
      }
    else
      break;
  }

  return ret;
}

RegSet& Instruction::explicit_maybe_write_set(RegSet& ret) const {
  if ((get_opcode() == Opcode::XCHG_EAX_R32 && get_operand<R32>(1) == eax) ||
      (get_opcode() == Opcode::XCHG_R32_EAX && get_operand<R32>(0) == eax)) {
    // xchg with eax hard-coded is essentially a nop
    return ret;
  }
  for (size_t i = 0, ie = arity(); i < ie; ++i) {
    if (maybe_extend(i))
      switch (type(i)) {
      case Type::EAX:
      case Type::R_32:
        ret += get_operand<R64>(i);
        break;
      case Type::XMM_0:
      case Type::XMM:
        ret += get_operand<Ymm>(i);
        break;
      default:
        assert(false);
        break;
      }
    else if (maybe_write(i))
      switch (type(i)) {
      case Type::MM:
        ret += get_operand<Mm>(i);
        break;
      case Type::RH:
        ret += get_operand<Rh>(i);
        break;
      case Type::AL:
      case Type::CL:
      case Type::R_8:
        ret += get_operand<R8>(i);
        break;
      case Type::AX:
      case Type::DX:
      case Type::R_16:
        ret += get_operand<R16>(i);
        break;
      case Type::EAX:
      case Type::R_32:
        ret += get_operand<R32>(i);
        break;
      case Type::RAX:
      case Type::R_64:
        ret += get_operand<R64>(i);
        break;
      case Type::FS:
      case Type::GS:
      case Type::SREG:
        ret += get_operand<Sreg>(i);
        break;
      case Type::ST_0:
      case Type::ST:
        ret += get_operand<St>(i);
        break;
      case Type::XMM_0:
      case Type::XMM:
        ret += get_operand<Xmm>(i);
        break;
      case Type::YMM:
        ret += get_operand<Ymm>(i);
        break;

      default:
        break;
      }
    else
      break;
  }

  return ret;
}

RegSet& Instruction::explicit_must_undef_set(RegSet& ret) const {
  for (size_t i = 0, ie = arity(); i < ie; ++i)
    if (must_undef(i))
      switch (type(i)) {
      case Type::MM:
        ret += get_operand<Mm>(i);
        break;
      case Type::RH:
        ret += get_operand<Rh>(i);
        break;
      case Type::AL:
      case Type::CL:
      case Type::R_8:
        ret += get_operand<R8>(i);
        break;
      case Type::AX:
      case Type::DX:
      case Type::R_16:
        ret += get_operand<R16>(i);
        break;
      case Type::EAX:
      case Type::R_32:
        ret += get_operand<R32>(i);
        break;
      case Type::RAX:
      case Type::R_64:
        ret += get_operand<R64>(i);
        break;
      case Type::FS:
      case Type::GS:
      case Type::SREG:
        ret += get_operand<Sreg>(i);
        break;
      case Type::ST_0:
      case Type::ST:
        ret += get_operand<St>(i);
        break;
      case Type::XMM_0:
      case Type::XMM:
        ret += get_operand<Ymm>(i);
        break;
      case Type::YMM:
        ret += get_operand<Ymm>(i);
        break;

      default:
        break;
      }
    else
      break;

  return ret;
}

RegSet& Instruction::explicit_maybe_undef_set(RegSet& ret) const {
  for (size_t i = 0, ie = arity(); i < ie; ++i)
    if (maybe_undef(i))
      switch (type(i)) {
      case Type::MM:
        ret += get_operand<Mm>(i);
        break;
      case Type::RH:
        ret += get_operand<Rh>(i);
        break;
      case Type::AL:
      case Type::CL:
      case Type::R_8:
        ret += get_operand<R8>(i);
        break;
      case Type::AX:
      case Type::DX:
      case Type::R_16:
        ret += get_operand<R16>(i);
        break;
      case Type::EAX:
      case Type::R_32:
        ret += get_operand<R32>(i);
        break;
      case Type::RAX:
      case Type::R_64:
        ret += get_operand<R64>(i);
        break;
      case Type::FS:
      case Type::GS:
      case Type::SREG:
        ret += get_operand<Sreg>(i);
        break;
      case Type::ST_0:
      case Type::ST:
        ret += get_operand<St>(i);
        break;
      case Type::XMM_0:
      case Type::XMM:
        ret += get_operand<Ymm>(i);
        break;
      case Type::YMM:
        ret += get_operand<Ymm>(i);
        break;

      default:
        break;
      }
    else
      break;

  return ret;
}

bool Instruction::check() const {
  // does this instruction have an operand that is greater or equal than 8 (like r10)
  // this includes registers that are nested inside a memory operand
  bool has_gp_gte_8 = false;
  // does this instruction have an rh operand
  bool has_rh = false;
  // does this instruction definitely require a rex prefix?
  assert((size_t)get_opcode() < rex_.size());
  bool definitely_require_rex = rex_[get_opcode()];
  // does this instruction have an r8 operand that is not al-dl?
  // does not include nested operands inside registers (because r8 cannot appear there)
  bool has_non_aldl_r8_operand = false;
  for (size_t i = 0, ie = arity(); i < ie; ++i)
    switch (type(i)) {
    case Type::HINT:
      if (!get_operand<Hint>(i).check()) {
        return false;
      }
      break;

    case Type::IMM_8:
      if (!get_operand<Imm8>(i).check()) {
        return false;
      }
      break;
    case Type::IMM_16:
      if (!get_operand<Imm16>(i).check()) {
        return false;
      }
      break;
    case Type::IMM_32:
      if (!get_operand<Imm32>(i).check()) {
        return false;
      }
      break;
    case Type::IMM_64:
      if (!get_operand<Imm64>(i).check()) {
        return false;
      }
      break;
    case Type::ZERO:
      if (!get_operand<Zero>(i).check()) {
        return false;
      }
      break;
    case Type::ONE:
      if (!get_operand<One>(i).check()) {
        return false;
      }
      break;
    case Type::THREE:
      if (!get_operand<Three>(i).check()) {
        return false;
      }
      break;

    case Type::LABEL:
      if (!get_operand<Label>(i).check()) {
        return false;
      }
      break;

    case Type::M_8:
    case Type::M_16:
    case Type::M_32:
    case Type::M_64:
    case Type::M_128:
    case Type::M_256:
    case Type::M_16_INT:
    case Type::M_32_INT:
    case Type::M_64_INT:
    case Type::M_32_FP:
    case Type::M_64_FP:
    case Type::M_80_FP:
    case Type::M_80_BCD:
    case Type::M_2_BYTE:
    case Type::M_28_BYTE:
    case Type::M_108_BYTE:
    case Type::M_512_BYTE:
    case Type::FAR_PTR_16_16:
    case Type::FAR_PTR_16_32:
    case Type::FAR_PTR_16_64:
      {
        auto mem_op = get_operand<M8>(i);
        if (!mem_op.check()) {
          return false;
        }
        if (mem_op.contains_base() && mem_op.get_base() >= 8) {
          has_gp_gte_8 = true;
        }
        if (mem_op.contains_index() && mem_op.get_index() >= 8) {
          has_gp_gte_8 = true;
        }
      }
      break;

    case Type::MM:
      if (!get_operand<Mm>(i).check()) {
        return false;
      }
      break;

    case Type::MOFFS_8:
    case Type::MOFFS_16:
    case Type::MOFFS_32:
    case Type::MOFFS_64:
      if (!get_operand<Moffs8>(i).check()) {
        return false;
      }
      break;

    case Type::PREF_66:
      if (!get_operand<Pref66>(i).check()) {
        return false;
      }
      break;
    case Type::PREF_REX_W:
      if (!get_operand<PrefRexW>(i).check()) {
        return false;
      }
      break;
    case Type::FAR:
      if (!get_operand<Far>(i).check()) {
        return false;
      }
      break;

    case Type::RH:
      if (!get_operand<Rh>(i).check()) {
        return false;
      }
      has_rh = true;
      break;
    case Type::AL:
      if (!get_operand<Al>(i).check()) {
        return false;
      }
      break;
    case Type::CL:
      if (!get_operand<Cl>(i).check()) {
        return false;
      }
      break;
    case Type::R_8:
      if (!get_operand<R8>(i).check()) {
        return false;
      }
      // we don't distinguish between has_gp_gte_8 and has_non_aldl_r8_operand here,
      // because setting either of them to true has the same effect
      if (get_operand<R16>(i) >= 4) {
        has_non_aldl_r8_operand = true;
      }
      break;
    case Type::AX:
      if (!get_operand<Ax>(i).check()) {
        return false;
      }
      break;
    case Type::DX:
      if (!get_operand<Dx>(i).check()) {
        return false;
      }
      break;
    case Type::R_16:
      if (!get_operand<R16>(i).check()) {
        return false;
      }
      if (get_operand<R16>(i) >= 8) {
        has_gp_gte_8 = true;
      }
      break;
    case Type::EAX:
      if (!get_operand<Eax>(i).check()) {
        return false;
      }
      break;
    case Type::R_32:
      if (!get_operand<R32>(i).check()) {
        return false;
      }
      if (get_operand<R32>(i) >= 8) {
        has_gp_gte_8 = true;
      }
      break;
    case Type::RAX:
      if (!get_operand<Rax>(i).check()) {
        return false;
      }
      break;
    case Type::R_64:
      if (!get_operand<R64>(i).check()) {
        return false;
      }
      if (get_operand<R64>(i) >= 8) {
        has_gp_gte_8 = true;
      }
      break;

    case Type::REL_8:
      if (!get_operand<Rel8>(i).check()) {
        return false;
      }
      break;
    case Type::REL_32:
      if (!get_operand<Rel32>(i).check()) {
        return false;
      }
      break;

    case Type::FS:
      if (!get_operand<Fs>(i).check()) {
        return false;
      }
      break;
    case Type::GS:
      if (!get_operand<Gs>(i).check()) {
        return false;
      }
      break;
    case Type::SREG:
      if (!get_operand<Sreg>(i).check()) {
        return false;
      }
      break;

    case Type::ST_0:
      if (!get_operand<St0>(i).check()) {
        return false;
      }
      break;
    case Type::ST:
      if (!get_operand<St>(i).check()) {
        return false;
      }
      break;

    case Type::XMM_0:
      if (!get_operand<Xmm0>(i).check()) {
        return false;
      }
      break;
    case Type::XMM:
      if (!get_operand<Ymm>(i).check()) {
        return false;
      }
      break;

    case Type::YMM:
      if (!get_operand<Ymm>(i).check()) {
        return false;
      }
      break;

    default:
      assert(false);
    }

  if (has_rh && (definitely_require_rex || has_gp_gte_8 || has_non_aldl_r8_operand)) {
    // such an instruction would both require to have a prefix byte,
    // and be required to not have one
    return false;
  }

  return true;
}

bool Instruction::operator<(const Instruction& rhs) const {
  if ( opcode_ != rhs.opcode_ )
    return opcode_ < rhs.opcode_;
  for ( size_t i = 0, ie = arity(); i < ie; ++i )
    switch ( type(i) ) {
    case Type::MOFFS_8:
    case Type::MOFFS_16:
    case Type::MOFFS_32:
    case Type::MOFFS_64:
      if ( operands_[i] != rhs.operands_[i] )
        return operands_[i] < rhs.operands_[i];
      break;
    default:
      if ( get_operand<R64>(i) != rhs.get_operand<R64>(i) )
        return get_operand<R64>(i) < rhs.get_operand<R64>(i);
      break;
    }
  return true;
}

bool Instruction::operator==(const Instruction& rhs) const {
  if ( opcode_ != rhs.opcode_ )
    return false;
  for ( size_t i = 0, ie = arity(); i < ie; ++i )
    switch ( type(i) ) {
    case Type::MOFFS_8:
    case Type::MOFFS_16:
    case Type::MOFFS_32:
    case Type::MOFFS_64:
      if ( operands_[i] != rhs.operands_[i] )
        return false;
      break;
    default:
      if ( get_operand<R64>(i) != rhs.get_operand<R64>(i) )
        return false;
      break;
    }
  return true;
}

size_t Instruction::hash() const {
  auto res = (size_t)opcode_;
  for ( size_t i = 0, ie = arity(); i < ie; ++i )
    switch ( type(i) ) {
    case Type::MOFFS_8:
    case Type::MOFFS_16:
    case Type::MOFFS_32:
    case Type::MOFFS_64:
      res ^= get_operand<Moffs8>(i).hash();
      break;
    default:
      res ^= get_operand<R64>(i).hash();
      break;
    }
  return res;
}

const array<size_t, X64ASM_NUM_OPCODES> Instruction::arity_ {{
    // Internal mnemonics
    1
    // Auto-generated mnemonics
#include "src/arity.table"
  }};

const array<array<Instruction::Properties, 4>, X64ASM_NUM_OPCODES> Instruction::properties_ {{
    // Internal mnemonics
    {Properties::none() + Property::MUST_READ, Properties::none(), Properties::none(), Properties::none()}
    // Auto-generated mnemonics
#include "src/properties.table"
  }};

const array<array<Type, 4>, X64ASM_NUM_OPCODES> Instruction::type_ {{
    // Internal mnemonics
    {{Type::LABEL}}
    // Auto-generated mnemonics
#include "src/type.table"
  }};

const array<int, X64ASM_NUM_OPCODES> Instruction::mem_index_ {{
    // Internal mnemonics
    -1
    // Auto-generated mnemonics
#include "src/mem_index.table"
  }};

const array<RegSet, X64ASM_NUM_OPCODES> Instruction::implicit_must_read_set_ {{
    // Internal mnemonics
    RegSet::empty()
    // Auto-generated mnemonics
#include "src/must_read.table"
  }};

const array<RegSet, X64ASM_NUM_OPCODES> Instruction::implicit_maybe_read_set_ {{
    // Internal mnemonics
    RegSet::empty()
    // Auto-generated mnemonics
#include "src/maybe_read.table"
  }};

const array<RegSet, X64ASM_NUM_OPCODES> Instruction::implicit_must_write_set_ {{
    // Internal mnemonics
    RegSet::empty()
    // Auto-generated mnemonics
#include "src/must_write.table"
  }};

const array<RegSet, X64ASM_NUM_OPCODES> Instruction::implicit_maybe_write_set_ {{
    // Internal mnemonics
    RegSet::empty()
    // Auto-generated mnemonics
#include "src/maybe_write.table"
  }};

const array<RegSet, X64ASM_NUM_OPCODES> Instruction::implicit_must_undef_set_ {{
    // Internal mnemonics
    RegSet::empty()
    // Auto-generated mnemonics
#include "src/must_undef.table"
  }};

const array<RegSet, X64ASM_NUM_OPCODES> Instruction::implicit_maybe_undef_set_ {{
    // Internal mnemonics
    RegSet::empty()
    // Auto-generated mnemonics
#include "src/maybe_undef.table"
  }};

const array<FlagSet, X64ASM_NUM_OPCODES> Instruction::flags_ {{
    // Internal mnemonics
    FlagSet::empty()
    // Auto-generatred mnemonics
#include "src/flag.table"
  }};

const array<size_t, X64ASM_NUM_OPCODES> Instruction::haswell_latency_ {{
    // Internal mnemonics
    0
    // Auto-generatred mnemonics
    #include "../codegen/haswell_latency.inc"
  }};

const array<bool, X64ASM_NUM_OPCODES> Instruction::rex_ {{
    // Internal mnemonics
    0
    // Auto-generated mnemonics
#include "src/rex.table"
  }};

} // namespace x64asm
