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

#include "src/op_type.h"
#include "src/operand.h"

namespace x64asm {

/** A segment register. The segment register bit assignments are ES = 0, 
	  CS = 1, SS = 2, DS = 3, FS = 4, and GS = 5.
*/
class Sreg : public AtomicOperand {
	friend class Constants;
	public:
		virtual constexpr bool check() {
			return val_ < 6;
		}
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
	protected:
		constexpr Sreg(uint64_t val) : AtomicOperand{val} { }
	private:
		virtual constexpr OpType type() {
			return OpType::SREG;
		}
};

/** The segment register FS. */
class Fs : public Sreg {
	friend class Constants;
	public:
		virtual constexpr bool check() {
			return val_ == 4;
		}
	private:
		constexpr Fs() : Sreg{4} { }
		virtual constexpr OpType type() {
			return OpType::FS;
		}
};

/** The segment register GS. */
class Gs : public Sreg {
	friend class Constants;
	public:
		virtual constexpr bool check() {
			return val_ == 5;
		}
	private:
		constexpr Gs() : Sreg{5} { }
		virtual constexpr OpType type() {
			return OpType::GS;
		}
};

} // namespace x64asm

#endif
