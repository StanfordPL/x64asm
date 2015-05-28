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

namespace {

  using namespace x64asm;

  typedef std::pair<x64asm::Opcode, std::vector<x64asm::Type>> Entry;
  typedef std::vector<Entry> Row;
  typedef std::map<std::string, Row> Table;

  Table att_table = {
    #include "src/att.table"
  };


  string trim(string s) {
    size_t first_char = s.find_first_not_of(' ');
    size_t last_char = s.find_last_not_of(' ');
    assert(!((first_char == string::npos) ^ (last_char == string::npos)));

    if(first_char == string::npos) {
      return "";
    } else {
      return s.substr(first_char, last_char - first_char + 1);
    }
  }

  /** Used by parser in deciding if an operand is memory or not. */
  bool is_mem(Type t) {
    return t == Type::M_8           || t == Type::M_16          || 
           t == Type::M_32          || t == Type::M_64          || 
           t == Type::M_128         || t == Type::M_256         ||
           t == Type::M_16_INT      || t == Type::M_32_INT      || 
           t == Type::M_64_INT      || t == Type::M_32_FP       || 
           t == Type::M_64_FP       || t == Type::M_80_FP       ||
           t == Type::M_80_BCD      || t == Type::M_2_BYTE      || 
           t == Type::M_28_BYTE     || t == Type::M_108_BYTE    || 
           t == Type::M_512_BYTE    || t == Type::FAR_PTR_16_16 ||
           t == Type::FAR_PTR_16_32 || t == Type::FAR_PTR_16_64;
  }

  /** Used in parsing to determine if an operand can be 
    rewritten to a given type. */
  bool is_a(const Operand* o, Type target) {
    Type parse = o->type();

    // These first two parses still have placeholder types.
    // They should be checked before the generic equality tests.
    if ( parse == Type::IMM_64 )
      switch ( target ) {
        case Type::ZERO:   return ((const Zero*)o)->check();
        case Type::ONE:    return ((const One*)o)->check();
        case Type::THREE:  return ((const Three*)o)->check();
        case Type::IMM_8:  return ((const Imm8*)o)->check();
        case Type::IMM_16: return ((const Imm16*)o)->check();
        case Type::IMM_32: return ((const Imm32*)o)->check();
        case Type::IMM_64: return ((const Imm64*)o)->check();
        default:           return false;
      }

    /*
    if ( parse == Type::MOFFS_8 ) {
      const auto offs = ((Moffs8*)o)->get_offset();
      if ( target == Type::MOFFS_8 || target == Type::MOFFS_16 ||
           target == Type::MOFFS_32 || target == Type::MOFFS_64 )
        return true;
      if ( is_mem(target) || target == Type::REL_32 )
        return ((const Imm32*)&offs)->check();
      if ( target == Type::REL_8 )
        return ((const Imm8*)&offs)->check();
    }
    */

    // Now it's alright to perform the generic checks.
    if ( parse == target )
      return true;
    if ( is_mem(parse) && is_mem(target) )
      return true;

    // Now try simple promotions.
    if ( parse == Type::AL && target == Type::R_8 )
      return true;
    if ( parse == Type::CL && target == Type::R_8 )
      return true;

    if ( parse == Type::AX && target == Type::R_16 )
      return true;
    if ( parse == Type::DX && target == Type::R_16 )
      return true;

    if ( parse == Type::EAX && target == Type::R_32 )
      return true;

    if ( parse == Type::RAX && target == Type::R_64 )
      return true;

    if ( parse == Type::FS && target == Type::SREG )
      return true;
    if ( parse == Type::GS && target == Type::SREG )
      return true;

    if ( parse == Type::ST_0 && target == Type::ST)
      return true;

    if ( parse == Type::XMM_0 && target == Type::XMM)
      return true;

    return false;
  }

} //namespace

namespace x64asm {

bool Instruction::is_xor_reg_reg() const {
  // we special case for xor of two identical registers.  in that case,
  // the instruction sets the value of the register to zero, and this is a
  // safe operation even if the register was undefined before.
  switch (get_opcode()) {
  case PXOR_MM_MM:
    if (get_operand<Mm>(0) == get_operand<Mm>(1)) {
      return true;
    }
    break;

  case PXOR_XMM_XMM:
    if (get_operand<Xmm>(0) == get_operand<Xmm>(1)) {
      return true;
    }
    break;

  case VPXOR_XMM_XMM_XMM:
    if (get_operand<Xmm>(1) == get_operand<Xmm>(2)) {
      return true;
    }
    break;

  case VPXOR_YMM_YMM_YMM:
    if (get_operand<Ymm>(1) == get_operand<Ymm>(2)) {
      return true;
    }
    break;

  case XOR_R8_R8:
    if (get_operand<R8>(0) == get_operand<R8>(1)) {
      return true;
    }
    break;

  case XOR_RH_RH:
    if (get_operand<Rh>(0) == get_operand<Rh>(1)) {
      return true;
    }
    break;

  case XOR_R16_R16:
    if (get_operand<R16>(0) == get_operand<R16>(1)) {
      return true;
    }
    break;

  case XOR_R32_R32:
    if (get_operand<R32>(0) == get_operand<R32>(1)) {
      return true;
    }
    break;

  case XOR_R64_R64:
    if (get_operand<R64>(0) == get_operand<R64>(1)) {
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



istream& Instruction::read_att(istream& is) {

  string line;
  getline(is, line);

  // Take care of comments
  size_t comment_index = line.find_first_of('#');
  string comment = "";
  if(comment_index != string::npos) {
    comment = trim(line.substr(comment_index+1));
    line = trim(line.substr(0, comment_index));
  }

  if(!line.size()) {
    is.setstate(ios::failbit);
    return is;
  }

  // Is it a label?
  regex is_label("\\.[a-zA-Z0-9_]+:");
  if(regex_match(line, is_label)) {
    string s = line.substr(0, line.size()-1);
    operands_ = {Label(s)};
    set_opcode(LABEL_DEFN);
    return is; 
  }

  // Parse opcode
  stringstream input(line);
  string opcode;

  input >> opcode;
  cout << "Read opcode: " << opcode << endl;

  // Parse operands
  std::vector<Operand> operands;
  while(input.good()) {
    cout << "attempting to read operand." << endl;
    Operand op = Constants::rax();
    input >> std::ws >> op;
    if(!input.fail()) {
      cout << "  Operand: " << op << endl;
      operands.insert(operands.begin(), op);
    } else {
      cerr << "  Error reading operand" << endl;
    }
    if(!input.eof()) {
      if(input.peek() == ',')
        input.ignore();
    }
  }

  // See if we can match this up to an instruction

  is.setstate(ios::failbit);
  return is;
}

ostream& Instruction::write_att(ostream& os) const {
  assert((size_t)get_opcode() < X64ASM_NUM_OPCODES);

  if (get_opcode() == LABEL_DEFN) {
    get_operand<Label>(0).write_att(os);
    os << ":";
    return os;
  }

  os << opcode_write_att(get_opcode()) << " ";
  if (arity() > 0)
    for (int i = (int)arity() - 1; i >= 0; --i) {
      switch (type(i)) {
      case Type::HINT:
        get_operand<Hint>(i).write_att(os);
        break;
      case Type::IMM_8:
      case Type::IMM_16:
      case Type::IMM_32:
      case Type::IMM_64:
      case Type::ZERO:
      case Type::ONE:
      case Type::THREE:
        get_operand<Three>(i).write_att(os);
        break;

      case Type::LABEL:
        get_operand<Label>(i).write_att(os);
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
        get_operand<FarPtr1664>(i).write_att(os);
        break;

      case Type::MM:
        get_operand<Mm>(i).write_att(os);
        break;

      case Type::MOFFS_8:
      case Type::MOFFS_16:
      case Type::MOFFS_32:
      case Type::MOFFS_64:
        get_operand<Moffs64>(i).write_att(os);
        break;

      case Type::PREF_66:
        get_operand<Pref66>(i).write_att(os);
        break;
      case Type::PREF_REX_W:
        get_operand<PrefRexW>(i).write_att(os);
        break;
      case Type::FAR:
        get_operand<Far>(i).write_att(os);
        break;

      case Type::RH:
        get_operand<Rh>(i).write_att(os);
        break;
      case Type::AL:
      case Type::CL:
      case Type::R_8:
        get_operand<R8>(i).write_att(os);
        break;
      case Type::AX:
      case Type::DX:
      case Type::R_16:
        get_operand<R16>(i).write_att(os);
        break;
      case Type::EAX:
      case Type::R_32:
        get_operand<R32>(i).write_att(os);
        break;
      case Type::RAX:
      case Type::R_64:
        get_operand<R64>(i).write_att(os);
        break;

      case Type::REL_8:
      case Type::REL_32:
        get_operand<Rel32>(i).write_att(os);
        break;

      case Type::FS:
      case Type::GS:
      case Type::SREG:
        get_operand<Sreg>(i).write_att(os);
        break;

      case Type::ST_0:
      case Type::ST:
        get_operand<St>(i).write_att(os);
        break;

      case Type::XMM_0:
      case Type::XMM:
        get_operand<Xmm>(i).write_att(os);
        break;

      case Type::YMM:
        get_operand<Ymm>(i).write_att(os);
        break;

      default:
        assert(false);
      }

      if (i != 0) {
        os << ", ";
      }
    }

  return os;
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
