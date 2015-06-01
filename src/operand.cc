
#include <sstream>
#include <iostream>

#include "src/fail.h"
#include "src/imm.h"
#include "src/m.h"
#include "src/operand.h"
#include "src/r.h"

using namespace x64asm;
using namespace std;
using namespace cpputil;

uint16_t Operand::size() const {

  switch(type()) {
  case Type::IMM_8:
  case Type::ZERO:
  case Type::ONE:
  case Type::THREE:
  case Type::M_8:
  case Type::MOFFS_8:
  case Type::RH:
  case Type::R_8:
  case Type::AL:
  case Type::CL:
    return 8;

  case Type::IMM_16:
  case Type::M_16:
  case Type::R_16:
  case Type::MOFFS_16:
  case Type::AX:
  case Type::DX:
    return 16;

  case Type::IMM_32:
  case Type::MOFFS_32:
  case Type::M_32:
  case Type::R_32:
  case Type::EAX:
    return 32;

  case Type::IMM_64:
  case Type::MOFFS_64:
  case Type::M_64:
  case Type::R_64:
  case Type::RAX:
    return 64;

  case Type::XMM:
  case Type::XMM_0:
  case Type::M_128:
    return 128;

  case Type::YMM:
  case Type::M_256:
    return 256;

  default:
    return 0;
  }
}

bool Operand::is_gp_register() const {

  switch(type()) {
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

bool Operand::is_sse_register() const {
  return type() == Type::XMM || type() == Type::YMM ||
         type() == Type::XMM_0;
}

bool Operand::is_typical_memory() const {

  switch(type()) {
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

bool Operand::is_immediate() const {

  switch(type()) {
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

istream& Operand::read_att(istream& is) {

  if(is.peek() == '*')
    is.ignore();

  // registers
  if(is.peek() == '%') {

    // read the register name
    stringstream name_builder;
    for(char c = is.peek(); c == '%' || c == ':' || ('a' <= c && c <= 'z') || ('0' <= c && c <= '9'); c = is.peek()) {
      is.ignore();
      name_builder.put(c);
    }
    string name(name_builder.str());
    stringstream tmp(name);

    // R?
    static_cast<R*>(this)->read_att(tmp);
    if(!tmp.fail())
      return is;

    // XMM?
    tmp.str(name);
    tmp.clear();
    static_cast<Xmm*>(this)->read_att(tmp);
    if(!tmp.fail())
      return is;

    // YMM?
    tmp.str(name);
    tmp.clear();
    static_cast<Ymm*>(this)->read_att(tmp);
    if(!tmp.fail())
      return is;

    // SREG?
    tmp.str(name);
    tmp.clear();
    static_cast<Sreg*>(this)->read_att(tmp);
    if(!tmp.fail())
      return is;


  } else if (is.peek() == '$') {
    // Immediates
    is.ignore();
    uint64_t value = 0;

    bool neg = false;
    if(is.peek() == '-') {
      is.ignore();
      neg = true;
    }

    if(is.peek() == '0') {
      is.ignore();
      if(is.peek() == 'x') {
        is.ignore();
        is >> hex >> value;
      } else {
        is >> dec >> value;
      }
    } else {
      is >> dec >> value;
    }

    if(neg)
      value = -value;

    Imm64 imm(value);
    *this = imm;

    return is;
  } else if (is.peek() == '.') {
    // Labels
    string name;
    is >> name;
    Label label(name);
    *this = label;
    return is;
  }

  // Memory?
  M8 m(Constants::rax());
  is >> m;
  *this = m;

  return is;

}

ostream& Operand::write_att(ostream& os) const {
  if(Operand::is_immediate()) {
    return static_cast<const Imm*>(this)->write_att(os);
  }
  if(Operand::is_typical_memory()) {
    return static_cast<const M<M8>*>(this)->write_att(os);
  }
  if(Operand::is_gp_register()) {
    return static_cast<const R*>(this)->write_att(os);
  }
  if(Operand::is_sse_register()) {
    return static_cast<const Sse*>(this)->write_att(os);
  }
  assert(false); //other Operands not supported for now.
  return os;
}

