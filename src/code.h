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

#ifndef X64ASM_SRC_CODE_H
#define X64ASM_SRC_CODE_H

#include <initializer_list>
#include <iostream>
#include <vector>

#include "src/flag_set.h"
#include "src/instruction.h"
#include "src/reg_set.h"

namespace x64asm {

/** A sequence of Instructions. In addition to the methods defined below, this
    class supports all of the behvaior of an STL sequence container.
*/
class Code : public std::vector<Instruction> {
public:
  /** Creates an empty code sequence. */
  Code() : std::vector<Instruction>() {}
  /** Creates a code sequence using initializer list syntax. */
  Code(const std::initializer_list<Instruction>& is) : std::vector<Instruction>(is) {}
  /** Creates a code sequence using the instruction in an stl container. */
  template <typename InItr>
  Code(InItr begin, InItr end) : std::vector<Instruction>(begin, end) {}

  /** Returns the set of cpu flags required to run this code. */
  FlagSet required_flags() const {
    auto fs = FlagSet::empty();
    for (const auto& instr : *this) {
      fs |= instr.required_flags();
    }
    return fs;
  }

  /** Returns the set of registers this code must read. */
  RegSet must_read_set() const {
    auto rs = RegSet::empty();
    auto mod = RegSet::empty();
    for (const auto& instr : *this) {
      rs |= (instr.must_read_set() - mod);
      mod |= (instr.maybe_write_set() | instr.maybe_undef_set());
    }
    return rs;
  }
  /** Returns the set of registers this code might read. */
  RegSet maybe_read_set() const {
    auto rs = RegSet::empty();
    auto mod = RegSet::empty();
    for (const auto& instr : *this) {
      rs |= (instr.maybe_read_set() - mod);
      mod |= (instr.maybe_write_set() | instr.maybe_undef_set());
    }
    return rs;
  }
  /** Returns the set of registers this code must write. */
  RegSet must_write_set() const {
    auto rs = RegSet::empty();
    for (const auto& instr : *this) {
      rs |= instr.must_write_set();
      rs -= instr.maybe_undef_set();
    }
    return rs;
  }
  /** Returns the set of registers this code might write. */
  RegSet maybe_write_set() const {
    auto rs = RegSet::empty();
    for (const auto& instr : *this) {
      rs |= instr.maybe_write_set();
    }
    return rs;
  }
  /** Returns the set of registers this code must undefine. */
  RegSet must_undef_set() const {
    auto rs = RegSet::empty();
    for (const auto& instr : *this) {
      rs |= instr.must_undef_set();
      rs -= instr.maybe_write_set();
    }
    return rs;
  }
  /** Returns the set of registers this code might undefine. */
  RegSet maybe_undef_set() const {
    auto rs = RegSet::empty();
    for (const auto& instr : *this) {
      rs |= instr.maybe_undef_set();
    }
    return rs;
  }

  /** Returns true iff every instruction is well-formed. */
  bool check() const {
    for (const auto & i : *this)
      if (!i.check()) {
        return false;
      }
    return true;
  }

  /** Reads a code sequence in at&t syntax from an istream. */
  std::istream& read_att(std::istream& is);
  /** Writes a code sequence to an ostream using at&t syntax. */
  std::ostream& write_att(std::ostream& os) const;

};

} // namespace x64asm

namespace std {

/** I/O overload. */
inline istream& operator>>(istream& is, x64asm::Code& c) {
  return c.read_att(is);
}
/** I/O overload. */
inline ostream& operator<<(ostream& os, const x64asm::Code& c) {
  return c.write_att(os);
}

} // namespace std

#endif
