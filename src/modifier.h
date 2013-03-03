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

#ifndef X64ASM_SRC_MODIFIER_H
#define X64ASM_SRC_MODIFIER_H

#include <iostream>

#include "src/operand.h"

namespace x64asm {

/** A modifier used to distinguish between mnemonics. These are non-standard
    operands which we have introduced to disambiguate parts of the intel x86_64
 	  specification.
*/
class Modifier : public Operand {
	public:
		/** Returns true if this modifier is well-formed. */
		constexpr bool check() {
			return val_ == 0;
		}

	protected:
		/** Direct access to this constructor is disallowed. */
		constexpr Modifier(uint64_t val) 
				: Operand{val} { 
		}
};

/** The 32-bit memory address override prefix: 0x66. */
class Pref66 : public Modifier {
	// Needs access to constructor.
	friend class Constants;

	public:
		/** Writes this modifier to an ostream using (something like) at&t syntax. */
		void write_att(std::ostream& os) const;
		/** Writes this modifier to an ostream using (something like) intel syntax. */
		void write_intel(std::ostream& os) const;

	private:
		/** Direct access to this constructor is disallowed. */
		constexpr Pref66() 
				: Modifier{0} { 
		}
};

/** The REX.w prefix: 0x48. */
class PrefRexW : public Modifier {
	// Needs access to constructor.
	friend class Constants;

	public:
		/** Writes this modifier to an ostream using (something like) at&t syntax. */
		void write_att(std::ostream& os) const;
		/** Writes this modifier to an ostream using (something like) intel syntax. */
		void write_intel(std::ostream& os) const;

	private:
		/** Direct access to this constructor is disallowed. */
		constexpr PrefRexW() 
				: Modifier{0} { 
		}
};

/** Far instruction variant. */
class Far : public Modifier {
	// Needs access to constructor.
	friend class Constants;

	public:
		/** Writes this modifier to an ostream using (something like) at&t syntax. */
		void write_att(std::ostream& os) const;
		/** Writes this modifier to an ostream using (something like) intel syntax. */
		void write_intel(std::ostream& os) const;

	private:
		/** Direct access to this constructor is disallowed. */
		constexpr Far() 
				: Modifier{0} { 
		}
};

} // namespace x64asm

#endif
