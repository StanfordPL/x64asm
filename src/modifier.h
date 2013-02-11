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

#include "src/op_type.h"
#include "src/operand.h"

namespace x64asm {

/** A modifier. */
class Modifier : public Operand {
	public:
		constexpr bool check() {
			return val_ == 0;
		}

	protected:
		constexpr Modifier(uint64_t val) 
				: Operand{val} { 
		}
};

/** The 32-bit memory address override prefix: 0x66. */
class Pref66 : public Modifier {
	friend class Constants;

	public:
		constexpr OpType type() {
			return OpType::PREF_66;
		}

		void write_att(std::ostream& os) const;
		void write_intel(std::ostream& os) const;

	private:
		constexpr Pref66() 
				: Modifier{0} { 
		}
};

/** The REX.w prefix: 0x48. */
class PrefRexW : public Modifier {
	friend class Constants;

	public:
		constexpr OpType type() {
			return OpType::PREF_REX_W;
		}

		void write_att(std::ostream& os) const;
		void write_intel(std::ostream& os) const;

	private:
		constexpr PrefRexW() 
				: Modifier{0} { 
		}
};

/** Far instruction variant. */
class Far : public Modifier {
	friend class Constants;

	public:
		constexpr OpType type() {
			return OpType::FAR;
		}

		void write_att(std::ostream& os) const;
		void write_intel(std::ostream& os) const;

	private:
		constexpr Far() 
				: Modifier{0} { 
		}
};

} // namespace x64asm

#endif
