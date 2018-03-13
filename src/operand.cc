
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

mutex Label::maps_mutex;

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

  // read the entire operand
  stringstream name_builder;
  size_t in_parens = 0;

  for(char c = is.peek(); ' ' <= c && c <= '~' && (c != ',' || in_parens); c = is.peek()) {
    /** This loop tokenizes the entire operand.  We're looking for any
     * non-whitespace character, to allow for lots of flexibility in labels.
     * The only trick is that if we encounter a ',' outside of parenthesis then
     * the operand needs to be terminated.  We need to allow whitespace in order
     * to accomodate things like "(%rsi, %rdi, 2)" */

    if(c == '(') {
      if(in_parens) {
        fail(is) << "x64 doesn't have nested parenthesis" << endl;
        return is;
      } else {
        in_parens = 1;
      }
    } 
    if(c == ')') {
      if(in_parens)
        in_parens = 0;
      else {
        fail(is) << "closeing ')' without opening" << endl;
        return is;
      }
    }

    is.ignore();
    name_builder.put(c);
  }
  string name(name_builder.str());
  stringstream tmp(name);

  char first_char = name[0];

  // registers
  if(first_char == '%') {

    // R?
    tmp.str(name);
    tmp.clear();
    static_cast<R*>(this)->read_att(tmp);
    if(!tmp.fail() && tmp.get() == EOF)
      return is;

    // XMM?
    tmp.str(name);
    tmp.clear();
    static_cast<Xmm*>(this)->read_att(tmp);
    if(!tmp.fail() && tmp.get() == EOF)
      return is;

    // YMM?
    tmp.str(name);
    tmp.clear();
    static_cast<Ymm*>(this)->read_att(tmp);
    if(!tmp.fail() && tmp.get() == EOF)
      return is;

    // MM?
    tmp.str(name);
    tmp.clear();
    static_cast<Mm*>(this)->read_att(tmp);
    if(!tmp.fail() && tmp.get() == EOF)
      return is;

    // SREG?
    tmp.str(name);
    tmp.clear();
    static_cast<Sreg*>(this)->read_att(tmp);
    if(!tmp.fail() && tmp.get() == EOF)
      return is;


  } else if (first_char == '$') {
    // Immediates
    tmp.ignore();
    uint64_t value = 0;

    bool neg = false;
    if(tmp.peek() == '-') {
      tmp.ignore();
      neg = true;
    }

    if(tmp.peek() == '0') {
      tmp.ignore();
      char c = tmp.peek();
      if(c == 'x') {
        tmp.ignore();
        tmp >> hex >> value;
      } else if ('0' <= c && c <= '9') {
        tmp >> dec >> value;
      } else {
        value = 0;
      }
    } else {
      tmp >> dec >> value;
    }

    if(neg)
      value = -value;

    Imm64 imm(value);
    *this = imm;

    return is;
  } else if (first_char == '.') {
    // Labels
    stringstream ss;
    ss << name;
    string label_name;
    ss >> label_name;
   
    Label label(label_name);
    *this = label;
    return is;
  }

  // Memory?
  tmp.str(name);
  tmp.clear();
  M8 m(Constants::rax());
  tmp >> m;
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

