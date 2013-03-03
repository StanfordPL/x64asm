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

#ifndef X64ASM_SRC_SREG_H
#define X64ASM_SRC_SREG_H

#include <iostream>

#include "src/operand.h"

namespace x64asm {

/** A segment register. The segment register bit assignments are ES = 0, 
	  CS = 1, SS = 2, DS = 3, FS = 4, and GS = 5.
*/
class Sreg : public Operand {
	// Needs access to constructor.
	friend class Constants;
	// Needs access to constructor.
	friend class M;
	// Needs access to constructor.
	friend class Moffs;

	public:
		/** Returns true if this segment register is well-formed. */
		constexpr bool check() {
			return val_ < 6;
		}

		/** Comaprison based on underlying type. */
		constexpr bool operator<(const Sreg& rhs) {
			return val_ < rhs.val_;
		}

		/** Comaprison based on underlying type. */
		constexpr bool operator==(const Sreg& rhs) {
			return val_ == rhs.val_;
		}

		/** Conversion based on underlying type. */
		constexpr operator uint64_t() {
			return val_;
		}

		/** Writes this segment register to an ostream using at&t syntax. */
		void write_att(std::ostream& os) const;
		/** Writes this segment register to an ostream using intel syntax. */
		void write_intel(std::ostream& os) const;

	protected:
		/** Direct access to this constructor is disallowed. */
		constexpr Sreg(uint64_t val) 
				: Operand{val} { 
		}
};

/** The segment register FS. */
class Fs : public Sreg {
	// Needs access to constructor.
	friend class Constants;

	public:
		/** Checks whether this segment register is %fs. */
		constexpr bool check() {
			return val_ == 4;
		}

	private:
		/** Direct access to this constructor is disallowed. */
		constexpr Fs() 
				: Sreg{4} { 
		}
};

/** The segment register GS. */
class Gs : public Sreg {
	// Needs access to constructor.
	friend class Constants;

	public:
		/** Checks whether this segment register is %gs. */
		constexpr bool check() {
			return val_ == 5;
		}

	private:
		/** Direct access to this constructor is disallowed. */
		constexpr Gs() 
				: Sreg{5} { 
		}
};

} // namespace x64asm

#endif
