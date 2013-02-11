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

#ifndef X64ASM_SRC_MM_H
#define X64ASM_SRC_MM_H

#include <iostream>

#include "src/operand.h"

namespace x64asm {

/** An MMX register. The 64-bit MMX registers are: MM0 through MM7. */
class Mm : public Operand {
	friend class Constants;

	public:
		constexpr bool check() {
			return val_ < 8;
		}

		void write_att(std::ostream& os) const;
		void write_intel(std::ostream& os) const;

	private:
		constexpr Mm(uint64_t val) : Operand{val} { }
};

} // namespace x64asm

#endif

