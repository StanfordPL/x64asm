/*
Copyright 2103 eric schkufza

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

/** A sequence of Instructions. */
class Code : public std::vector<Instruction> {
	public:
		Code()
				: std::vector<Instruction>{} { 
		}

		Code(std::initializer_list<Instruction> is) 
				: std::vector<Instruction>{is} { 
		}

		template <typename InItr>
		Code(InItr begin, InItr end) 
				: std::vector<Instruction>{begin, end} { 
		}

		bool check() const;

		void read_att(std::istream& is);
		void read_intel(std::istream& is);

		void write_att(std::ostream& os) const;
		void write_intel(std::ostream& os) const;
};

} // namespace x64asm

#endif
