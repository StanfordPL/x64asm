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
	// Needs access to constructor.
	friend class Constants;

	public:
		/** Returns true if this is a valid mmx register. */
		constexpr bool check() {
			return val_ < 8;
		}

		/** Comparison based on underlying value. */
		constexpr bool operator<(const Mm& rhs) {
			return val_ < rhs.val_;
		}

		/** Comparison based on underlying value. */
		constexpr bool operator==(const Mm& rhs) {
			return val_ == rhs.val_;
		}

		/** Conversion based on underlying value. */
		constexpr operator uint64_t() {
			return val_;
		}

		/** Writes this mmx register to an ostream using at&t syntax. */
		void write_att(std::ostream& os) const;
		/** Writes this mmx register to an ostream using intel syntax. */
		void write_intel(std::ostream& os) const;

	private:
		/** Direct access to this contructor is disallowed. */
		constexpr Mm(uint64_t val) : Operand{val} { }
};

} // namespace x64asm

#endif

