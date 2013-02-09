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

#ifndef X64ASM_SRC_YMM_H
#define X64ASM_SRC_YMM_H

#include <iostream>

#include "src/op_type.h"
#include "src/operand.h"

namespace x64asm {

/** A YMM register. The 256-bit YMM registers are: YMM0 through YMM7; YMM8 
	  through YMM15 are available in 64-bit mode.
*/
class Ymm : public Operand {
	friend class Constants;
	friend class Xmm;
	public:
		virtual constexpr bool check() {
			return val_ < 16;
		}
		virtual constexpr OpType type() {
			return OpType::YMM;
		}
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
	private:
		constexpr Ymm(uint64_t val) : Operand{val} { } 
		virtual void insert_in(RegSet& os, bool promote = false) const;
};

} // namespace x64asm

#endif

