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

#ifndef X64ASM_SRC_CODE_H
#define X64ASM_SRC_CODE_H

#include <initializer_list>
#include <iostream>
#include <vector>

#include "src/instruction.h"

namespace x64asm {

/** A sequence of Instructions. In addition to the methods defined below, this
    class supports all of the behvaior of an STL sequence container.
*/
class Code : public std::vector<Instruction> {
  public:
    /** Creates an empty code sequence. */
    Code();
    /** Creates a code sequence using initializer list syntax. */
    Code(const std::initializer_list<Instruction>& is);
    /** Creates a code sequence using the instruction in an stl container. */
    template <typename InItr>
    Code(InItr begin, InItr end);

    /** Returns true iff every instruction is well-formed. */
    bool check() const;

    /** Reads a code sequence in at&t syntax from an istream. */
    std::istream& read_att(std::istream& is);
    /** Writes a code sequence to an ostream using at&t syntax. */
    std::ostream& write_att(std::ostream& os) const;
};

} // namespace x64asm

namespace std {

/** I/O overload. */
istream& operator>>(istream& is, x64asm::Code& c);
/** I/O overload. */
ostream& operator<<(ostream& os, const x64asm::Code& c);

} // namespace std

namespace x64asm {

inline Code::Code() :
    std::vector<Instruction> {} {
}

inline Code::Code(const std::initializer_list<Instruction>& is) : 
    std::vector<Instruction> {is} {
}

template <typename InItr>
inline Code::Code(InItr begin, InItr end) : 
    std::vector<Instruction> {begin, end} {
}

inline bool Code::check() const {
  for (const auto & i : *this)
    if (!i.check()) {
      return false;
    }
  return true;
}

inline std::ostream& Code::write_att(std::ostream& os) const {
  for (size_t i = 0, ie = size(); i < ie; ++i) {
    (*this)[i].write_att(os);
    if (i+1 != ie) {
      os << std::endl;
    }
  }
  return os;
}

} // namespace x64asm

namespace std {

inline istream& operator>>(istream& is, x64asm::Code& c) {
  return c.read_att(is);
}

inline ostream& operator<<(ostream& os, const x64asm::Code& c) {
  return c.write_att(os);
}

} // namespace std

#endif
