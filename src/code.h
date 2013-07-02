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
    Code()
      : std::vector<Instruction> {} {
    }

    /** Creates a code sequence using initializer list syntax. */
    Code(const std::initializer_list<Instruction>& is)
      : std::vector<Instruction> {is} {
    }

    /** Creates a code sequence using the instruction in an stl container. */
    template <typename InItr>
    Code(InItr begin, InItr end)
      : std::vector<Instruction> {begin, end} {
    }

    /** Returns true iff each of the instruction in this sequence are
    	  well-formed.
    */
    bool check() const;

    /** Reads a code sequence in at&t syntax from an istream. */
    void read_att(std::istream& is);
    /** Writes a code sequence to an ostream using at&t syntax. */
    void write_att(std::ostream& os) const;
};

} // namespace x64asm

#endif
