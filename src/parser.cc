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


#include "src/assembler.h"
#include "src/constants.h"
#include "src/fail.h"
#include "src/instruction.h"
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
using namespace cpputil;

// #define DEBUG_PARSER

namespace {

  using namespace x64asm;

  typedef std::vector<uint16_t> ImmSizes;
  typedef uint16_t TargetSize;
  typedef std::pair<TargetSize, ImmSizes> SizeInfo;
  typedef std::pair<x64asm::Opcode, std::vector<x64asm::Type>> Entry;
  typedef std::pair<SizeInfo, std::vector<Entry>> Row;
  typedef std::map<std::string, Row> Table;

  Table att_table = {
    #include "src/att.table"
  };


  string trim(string s) {
    size_t first_char = s.find_first_not_of(" \t");
    size_t last_char = s.find_last_not_of(" \t");
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


  size_t number_leading_zeros(uint64_t value) {
    size_t num_zeroes = 0;
    uint64_t one = 1;
    for(int i = 63; i >= 0; --i) {
      if ((value & (one << i)) == 0)
        ++num_zeroes;
      else
        break;
    }
    return num_zeroes;
  }


  /** Used in parsing to determine if an operand can be 
    rewritten to a given type. */
  bool is_a(const Operand* o, Type target, SizeInfo& size_info) {
    Type parse = o->type();

    // These first two parses still have placeholder types.
    // They should be checked before the generic equality tests.
    if ( parse == Type::IMM_64 ) {
      switch ( target ) {
        case Type::ZERO:   return ((const Zero*)o)->check();
        case Type::ONE:    return ((const One*)o)->check();
        case Type::THREE:  return ((const Three*)o)->check();
        case Type::IMM_8:
        case Type::IMM_16:
        case Type::IMM_32: {
          // target_w is the width that the operand will be sign-extended to (if necessary)
          uint16_t target_w = size_info.first;
          ImmSizes imm_sizes = size_info.second;
          uint16_t imm_max = *max_element(imm_sizes.begin(), imm_sizes.end());
          uint16_t w = bit_width_of_type(target);
          uint64_t bits = *((const Imm64*)o);

          // check the bits that are to the left of the biggest possible immediate.  they all need to be 0 or 1.
          uint64_t bits_above = 0;
          if (imm_max != 64) bits_above =((-1LL << imm_max) & bits) >> imm_max;
          if (bits_above == 0) {
            // all zeros above, that's fine
          } else if (bits_above == (1ULL << (64 - imm_max)) - 1) {
            // all ones, check that msb is also 1
            if ((bits >> (imm_max - 1)) & 0x1 == 0x1) {
              // also one, ok
            } else {
              return false;
            }
          } else {
            return false;
          }

          // if this is the only imm size, or if it is the widest imm size, we can accept
          if (w == imm_max) return true;

          // replicate sign bit
          int64_t se_bits = bits;
          if (imm_max != 64) se_bits = (se_bits << (64-imm_max)) >> (64-imm_max);

          auto n_leading_ones = number_leading_zeros(~se_bits);
          auto n_leading_zeros = number_leading_zeros(se_bits);

          // is this a positive constant that fits within w bits?
          if (n_leading_zeros > 64-w) return true;

          // is this a negative constants that fits within w bits?
          if (n_leading_ones >= 64-w) return true;

          // does not fit
          return false;
        }
        case Type::IMM_64: return true;
        default:           return false;
      }
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

/** Promotes an operand to a given type.  */
void Instruction::promote(Operand* op, Type target) {

  switch(target) {
    case Type::IMM_8: {
      uint64_t value = (uint64_t)*static_cast<Imm64*>(op);
      *op = Imm8(value & 0xff);
      return;
    }
    case Type::IMM_16: {
      uint64_t value = (uint64_t)*static_cast<Imm64*>(op);
      *op = Imm16(value & 0xffff);
      return;
    }
    case Type::IMM_32: {
      uint64_t value = (uint64_t)*static_cast<Imm64*>(op);
      *op = Imm32(value & 0xffffffff);
      return;
    }
    default:
      op->set_type_maybe_unless_I_know_better_hack(target);
      return;
  }

}

istream& Instruction::read_att(istream& is) {

  string line;
  getline(is, line);

#ifdef DEBUG_PARSER
  cout << ":: trying to parse '" << line << "'" << endl;
#endif

  // Take care of comments
  size_t comment_index = line.find_first_of('#');
  string comment = "";
  if(comment_index != string::npos) {
    comment = trim(line.substr(comment_index+1));
    line = line.substr(0, comment_index);
  }

  // Trim and handle empty lines
  line = trim(line);

  if(!line.size()) {
    fail(is) << "line was empty";
    return is;
  }

  // Is it a label?
  if(line[0] == '.' && line[line.size() - 1] == ':') {
    string s = line.substr(0, line.size()-1);
    operands_ = {Label(s)};
    set_opcode(LABEL_DEFN);
    return is;
  }

  // Parse opcode
  stringstream input(line);
  string opcode;

  input >> opcode;

  // Parse operands
  std::vector<Operand> operands;
  input >> std::ws;
  size_t operand_count = 1;
  while(input.good()) {
    Operand op = Constants::rax();
    input >> op;
    if(failed(input)) {
      fail(is) << "Could not parse operand " << operand_count << ": " << fail_msg(input);
      return is;
    }
    operands.insert(operands.begin(), op);
    operand_count++;
    if(!input.eof()) {
      if(input.peek() == ',') {
        input.ignore();
        input >> std::ws;
      } else if(!input.eof()) {
        fail(is) << "Expected ',' but found '" << (char)input.peek() << "'";
        return is;
      }
    } 
  }

  // Check for annotations in the comment
  size_t opc_pos = comment.find("OPC=");
  Opcode expected_opcode = (Opcode)0;
  if(opc_pos != string::npos) {
    string opcode_text = comment.substr(opc_pos+4);
    stringstream ss(opcode_text);
    ss >> expected_opcode;
    if(ss.fail())
      expected_opcode = (Opcode)0;
  }

  size_t size_pos = comment.find("SIZE=");
  uint64_t expected_size = 0;
  if(size_pos != string::npos) {
    expected_size = strtoull(comment.c_str() + size_pos + 5, NULL, 10);
  }

  // See if we can match this up to an instruction
  auto data = att_table[opcode];
  std::vector<Entry> possible_encodings = data.second;
  if(possible_encodings.size() == 0) {
    fail(is) << "No such opcode '" << opcode << "' exists";
    return is;
  }

  for(auto entry : possible_encodings) {

    if(expected_opcode && entry.first != expected_opcode) {
      continue;
    }

    if(entry.second.size() != operands.size())
      continue;

#ifdef DEBUG_PARSER
    cout << ":: trying to use '" << entry.first << "'" << endl;
#endif

    bool works = true;
    for(size_t i = 0; i < entry.second.size(); ++i)
      works &= is_a(&operands[i], entry.second[i], data.first);
#ifdef DEBUG_PARSER
    if (!works) cout << ":: does not work because of operand types" << endl;
#endif
    if(!works)
      continue;

    for(size_t i = 0; i < entry.second.size(); ++i) {
      operands_[i] = operands[i];
      promote(&operands_[i], entry.second[i]);
    }

    set_opcode(entry.first);

    if(expected_size) {
      Assembler assm;
      Code c({*this});
      auto fxn = assm.assemble(c).second;
      size_t actual = fxn.size();
      if(actual != expected_size) {
#ifdef DEBUG_PARSER
        cout << ":: expected size " << expected_size << ", but size is " << actual << "." << endl;
#endif
        continue;
      }
    }

#ifdef DEBUG_PARSER
    cout << ":: parsed as '" << (*this) << "'" << endl;
#endif
    return is;
  }

#ifdef DEBUG_PARSER
  cout << ":: failed to parse" << endl;
#endif
  fail(is) << "Could not match opcode/operands to an x86-64 instruction";

  return is;
}


ostream& Instruction::write_att(ostream& os) const {
  assert((size_t)get_opcode() < X64ASM_NUM_OPCODES);

  if (get_opcode() == LABEL_DEFN) {
    get_operand<Label>(0).write_att(os);
    os << ":";
    return os;
  }

  string opcode = opcode_write_att(get_opcode());
  os << opcode;
  if (arity() > 0)
    os << " ";
    for (int i = (int)arity() - 1; i >= 0; --i) {
      switch (type(i)) {
      case Type::HINT:
        get_operand<Hint>(i).write_att(os);
        break;
      case Type::IMM_8:
      case Type::IMM_16:
      case Type::IMM_32: {
        auto data = att_table[opcode];
        ImmSizes imm_sizes = data.first.second;
        uint16_t imm_max = *max_element(imm_sizes.begin(), imm_sizes.end());
        uint16_t w = bit_width_of_type(type(i));
        uint64_t bits = get_operand<Imm64>(i);

        // replicate sign bit
        int64_t se_bits = bits;
        if (w != 64) se_bits = (se_bits << (64-w)) >> (64-w);

        // cut off anything beyond imm_max
        if (imm_max != 64) se_bits = se_bits & ((1ULL << imm_max) - 1);

        const auto fmt = os.flags();
        os << "$0x" << std::noshowbase << std::hex << se_bits;
        os.flags(fmt);
        break;
      }
      case Type::IMM_64:
        get_operand<Three>(i).write_att(os);
        break;
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


} // namespace x64asm
