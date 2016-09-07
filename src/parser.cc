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

namespace {

  using namespace x64asm;

  typedef std::pair<x64asm::Opcode, std::vector<x64asm::Type>> Entry;
  typedef std::vector<Entry> Row;
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

/** Promotes an operand to a given type.  Returns 'true' if 
  the fit is dangerous and would best be avoided; e.g. if 0x80..0xff are put
  into an 8-bit immediate, this could mess up the sign.  We can assume is_a()
  on the same arguments has returned true.  */
bool Instruction::promote(Operand* op, Type target) {

  switch(target) {
    case Type::IMM_8: {
      uint64_t value = (uint64_t)*static_cast<Imm64*>(op);
      *op = Imm8(value & 0xff);
      return (0x80 <= value && value <= 0xff) || (0xffffffffffffff00 <= value && value <= 0xffffffffffffff80);
    }
    case Type::IMM_16: {
      uint64_t value = (uint64_t)*static_cast<Imm64*>(op);
      *op = Imm16(value & 0xffff);
      return (0x8000 <= value && value <= 0xffff) || (0xffffffffffff0000 <= value && value <= 0xffffffffffff8000);
    }
    case Type::IMM_32: {
      uint64_t value = (uint64_t)*static_cast<Imm64*>(op);
      *op = Imm32(value & 0xffffffff);
      return (0x80000000 <= value && value <= 0xffffffff) || (0xffffffff00000000 <= value && value <= 0xffffffff80000000);
    }
    default:
      op->set_type_maybe_unless_I_know_better_hack(target);
      return false;
  }

}



istream& Instruction::read_att(istream& is) {

  string line;
  getline(is, line);

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
  bool found_poor = false;
  Row possible_encodings = att_table[opcode];
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

    bool works = true;
    for(size_t i = 0; i < entry.second.size(); ++i)
      works &= is_a(&operands[i], entry.second[i]);
    if(!works)
      continue;

    bool poor = false;
    for(size_t i = 0; i < entry.second.size(); ++i) {
      operands_[i] = operands[i];
      poor |= promote(&operands_[i], entry.second[i]);
    }

    set_opcode(entry.first);

    if(expected_size) {
      Assembler assm;
      Code c({*this});
      auto fxn = assm.assemble(c).second;
      size_t actual = fxn.size();
      if(actual != expected_size)
        continue;
    }

    if(!poor)
      return is;
    else 
      found_poor = true;
  }

  if(!found_poor) {
    fail(is) << "Could not match opcode/operands to an x86-64 instruction";
  }

  return is;
}

} // namespace x64asm
