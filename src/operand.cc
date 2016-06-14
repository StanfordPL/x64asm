
#include <sstream>
#include <iostream>

#include "src/fail.h"
#include "src/imm.h"
#include "src/label.h"
#include "src/type.h"
#include "src/m.h"
#include "src/operand.h"
#include "src/r.h"

using namespace x64asm;
using namespace std;
using namespace cpputil;

uint64_t Operand::uid_counter_ = 0;

uint16_t Operand::size() const {
  return bit_width_of_type(type());
}

bool Operand::is_gp_register() const {
  return is_type_gp_register(type());
}

bool Operand::is_sse_register() const {
  return is_type_sse_register(type());
}

bool Operand::is_mm_register() const {
  return is_type_mm_register(type());
}

bool Operand::is_typical_memory() const {
  return is_type_typical_memory(type());
}

bool Operand::is_immediate() const {
  return is_type_immediate(type());
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

    // MM?
    tmp.str(name);
    tmp.clear();
    static_cast<Mm*>(this)->read_att(tmp);
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
      char c = is.peek();
      if(c == 'x') {
        is.ignore();
        is >> hex >> value;
      } else if ('0' <= c && c <= '9') {
        is >> dec >> value;
      } else {
        value = 0;
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
  if(Operand::is_mm_register()) {
    return static_cast<const Mm*>(this)->write_att(os);
  }
  assert(false); //other Operands not supported for now.
  return os;
}

