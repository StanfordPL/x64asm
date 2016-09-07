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

#include "src/fail.h"
#include "src/r.h"

#include <array>
#include <cassert>
#include <string>

#include "src/constants.h"

using namespace cpputil;
using namespace std;
using namespace x64asm;

namespace {

constexpr array<const char*, 4> rhs_() {
  return {
    "%ah", "%ch", "%dh", "%bh"
  };
}

constexpr array<const char*, 16> r8s_() {
  return {
    "%al", "%cl", "%dl", "%bl", "%spl", "%bpl", "%sil", "%dil",
    "%r8b", "%r9b", "%r10b", "%r11b", "%r12b", "%r13b", "%r14b", "%r15b"
  };
}

constexpr array<const char*, 16> r16s_() {
  return {
    "%ax", "%cx", "%dx", "%bx", "%sp", "%bp", "%si", "%di",
    "%r8w", "%r9w", "%r10w", "%r11w", "%r12w", "%r13w", "%r14w", "%r15w"
  };
}

constexpr array<const char*, 16> r32s_() {
  return {
    "%eax", "%ecx", "%edx", "%ebx", "%esp", "%ebp", "%esi", "%edi",
    "%r8d", "%r9d", "%r10d", "%r11d", "%r12d", "%r13d", "%r14d", "%r15d"
  };
}

constexpr array<const char*, 16> r64s_() {
  return {
    "%rax", "%rcx", "%rdx", "%rbx", "%rsp", "%rbp", "%rsi", "%rdi",
    "%r8", "%r9", "%r10", "%r11", "%r12", "%r13", "%r14", "%r15"
  };
}

} // namespace

namespace x64asm {

istream& R8::read_att(istream& is) {
  string temp;
  is >> temp;

  for (size_t i = 0; i < r8s_().size(); ++i) {
    if (temp == r8s_()[i]) {
      *this = Constants::r8s()[i];
      return is;
    }
  }

  fail(is) << "No such r8 register \"" << temp << "\"";
  return is;
}

ostream& R8::write_att(ostream& os) const {
  assert(check());
  return (os << r8s_()[val_]);
}

istream& Rh::read_att(istream& is) {
  string temp;
  is >> temp;

  for (size_t i = 0, ie = rhs_().size(); i < ie; ++i) {
    if (temp == rhs_()[i]) {
      *this = Constants::rhs()[i];
      return is;
    }
  }

  fail(is) << "No such rh register \"" << temp << "\"";
  return is;
}

ostream& Rh::write_att(ostream& os) const {
  assert(check());
  return (os << rhs_()[val_-4]);
}

istream& R16::read_att(istream& is) {
  string temp;
  is >> temp;

  for (size_t i = 0, ie = r16s_().size(); i < ie; ++i) {
    if (temp == r16s_()[i]) {
      *this = Constants::r16s()[i];
      return is;
    }
  }

  fail(is) << "No such r16 register \"" << temp << "\"";
  return is;
}

ostream& R16::write_att(ostream& os) const {
  assert(check());
  return (os << r16s_()[val_]);
}

istream& R32::read_att(istream& is) {
  string temp;
  is >> temp;

  for (size_t i = 0, ie = r32s_().size(); i < ie; ++i) {
    if (temp == r32s_()[i]) {
      *this = Constants::r32s()[i];
      return is;
    }
  }

  fail(is) << "No such r32 register \"" << temp << "\"";
  return is;
}

ostream& R32::write_att(ostream& os) const {
  assert(check());
  return (os << r32s_()[val_]);
}

istream& R64::read_att(istream& is) {
  string temp;
  is >> temp;

  for (size_t i = 0, ie = r64s_().size(); i < ie; ++i) {
    if (temp == r64s_()[i]) {
      *this = Constants::r64s()[i];
      return is;
    }
  }

  fail(is) << "No such r64 register \"" << temp << "\"";
  return is;
}

ostream& R64::write_att(ostream& os) const {
  assert(check());
  return (os << r64s_()[val_]);
}

istream& R::read_att(istream& is) {

  string name;
  is >> name;

  stringstream tmp;

  // R32?
  tmp.str(name);
  tmp.clear();
  static_cast<R32*>(this)->read_att(tmp);
  if(!tmp.fail())
    return is;

  // R64?
  tmp.str(name);
  tmp.clear();
  static_cast<R64*>(this)->read_att(tmp);
  if(!tmp.fail())
    return is;

  // R8?
  tmp.str(name);
  tmp.clear();
  static_cast<R8*>(this)->read_att(tmp);
  if(!tmp.fail())
    return is;

  // RH?
  tmp.str(name);
  tmp.clear();
  static_cast<Rh*>(this)->read_att(tmp);
  if(!tmp.fail())
    return is;

  // R16?
  tmp.str(name);
  tmp.clear();
  static_cast<R16*>(this)->read_att(tmp);
  if(!tmp.fail())
    return is;

  fail(is) << "No such general-purpose register \"" << name << "\"" << endl;
  return is;

}


ostream& R::write_att(ostream& os) const {
  switch(type()) {
  case Type::RH:
    return static_cast<const Rh* const>(this)->write_att(os);
    break;

  case Type::R_8:
  case Type::AL:
  case Type::CL:
    return static_cast<const R8* const>(this)->write_att(os);
    break;

  case Type::R_16:
  case Type::AX:
  case Type::DX:
    return static_cast<const R16 * const>(this)->write_att(os);
    break;

  case Type::R_32:
  case Type::EAX:
    return static_cast<const R32 * const>(this)->write_att(os);
    break;

  case Type::R_64:
  case Type::RAX:
    return static_cast<const R64 * const>(this)->write_att(os);
    break;

  default:
    assert(false);
    return os;
    break;
  }
}

} // namespace x64asm
