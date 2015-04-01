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

#ifndef X64ASM_SRC_STL_H
#define X64ASM_SRC_STL_H

#include "src/code.h"
#include "src/flag.h"
#include "src/flag_set.h"
#include "src/function.h"
#include "src/instruction.h"
#include "src/opcode.h"
#include "src/reg_set.h"
#include "src/type_traits.h"

namespace std {

// operator>> overloads

inline istream& operator>>(istream& is, x64asm::Function& f) {
  return f.read_hex(is);
}

inline istream& operator>>(istream& is, x64asm::Opcode& o) {
  return read_att(is, o);
}

template <typename T>
inline typename enable_if<
    is_same<T, x64asm::Code>::value ||
    is_same<T, x64asm::Instruction>::value ||
    x64asm::is_operand<T>::value,
    istream&>::type
operator>>(istream& is, T& t) {
  return t.read_att(is);
}

template <typename T>
inline typename enable_if<
    x64asm::is_env_bits<T>::value ||
    x64asm::is_env_reg<T>::value ||
    is_same<T, x64asm::FlagSet>::value ||
    is_same<T, x64asm::RegSet>:: value,
    istream&>::type
operator>>(istream& is, T& t) {
  return t.read_text(is);
}

inline istream& operator>>(istream& is, x64asm::Flag& f) {
  return read_text(is, f);
}

// operator<< overloads

inline ostream& operator<<(ostream& os, const x64asm::Function& f) {
  return f.write_hex(os);
}

inline ostream& operator<<(ostream& os, const x64asm::Opcode& o) {
  return write_att(os, o);
}

template <typename T>
inline typename enable_if<
    is_same<T, x64asm::Code>::value ||
    is_same<T, x64asm::Instruction>::value ||
    x64asm::is_operand<T>::value,
    ostream&>::type
operator<<(ostream& os, const T& t) {
  return t.write_att(os);
}

inline ostream& operator<<(ostream& os, const x64asm::Flag& f) {
  return write_text(os, f);
}

template <typename T>
inline typename enable_if<
    x64asm::is_env_bits<T>::value ||
    x64asm::is_env_reg<T>::value ||
    is_same<T, x64asm::FlagSet>::value ||
    is_same<T, x64asm::RegSet>:: value,
    ostream&>::type
operator<<(ostream& os, const T& t) {
  return t.write_text(os);
}

// hash specialization

template <typename T>
struct hash {
  typename enable_if<
      is_same<T, x64asm::Function>::value ||
      is_same<T, x64asm::Code>::value ||
      is_same<T, x64asm::Instruction>::value ||
      x64asm::is_operand<T>::value ||
      x64asm::is_env_bits<T>::value ||
      x64asm::is_env_reg<T>::value ||
      is_same<T, x64asm::FlagSet>::value ||
      is_same<T, x64asm::RegSet>:: value,
      size_t>::type
  operator()(const T& t) const {
    return t.hash();
  }
};

} // namespace std

#endif
