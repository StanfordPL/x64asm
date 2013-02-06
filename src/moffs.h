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

#ifndef X64ASM_SRC_MOFFS_H
#define X64ASM_SRC_MOFFS_H

#include <iostream>

#include "src/op_type.h"
#include "src/operand.h"
#include "src/sreg.h"
#include "src/imm.h"

namespace x64asm {

/** A simple memory variable. */
class Moffs : public CompoundOperand {
	public:
		bool contains_seg() const {
			return seg_ != 0;
		}

		const Sreg* get_seg() const {
			return seg_;
		}

		const Imm64* get_offset() const {
			return offset_;
		}

		void set_seg(const Sreg* seg) {
			seg_ = seg;
		}

		void set_offset(const Imm64* offset) {
			offset_ = offset;
		}

		void clear_seg() {
			seg_ = 0;
		}

		virtual bool check() const; 
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;

	protected:
		constexpr Moffs(const Sreg* seg, const Imm64* offset) 
				: seg_{seg}, offset_{offset} { 
		}
		constexpr Moffs(const Imm64* offset) 
				: seg_{0}, offset_{offset} { 
		}

	private:
		const Sreg* seg_;
		const Imm64* offset_;
};

/** A simple memory variable (memory offset) of type byte. */
class Moffs8 : public Moffs {
	public:
		constexpr Moffs8(const Sreg* seg, const Imm64* offset) 
				: Moffs{seg, offset} { 
		}
		constexpr Moffs8(const Imm64* offset) 
				: Moffs{offset} { 
		}
		virtual constexpr OpType type() {
			return OpType::MOFFS_8;
		}
};

/** A simple memory variable (memory offset) of type word. */
class Moffs16 : public Moffs {
	public:
		constexpr Moffs16(const Sreg* seg, const Imm64* offset) 
				: Moffs{seg, offset} { 
		}
		constexpr Moffs16(const Imm64* offset) 
				: Moffs{offset} { 
		}
		virtual constexpr OpType type() {
			return OpType::MOFFS_16;
		}
};

/** A simple memory variable (memory offset) of type doubleword. */
class Moffs32 : public Moffs {
	public:
		constexpr Moffs32(const Sreg* seg, const Imm64* offset) 
				: Moffs{seg, offset} { 
		}
		constexpr Moffs32(const Imm64* offset) 
				: Moffs{offset} { 
		}
		virtual constexpr OpType type() {
			return OpType::MOFFS_32;
		}
};

/** A simple memory variable (memory offset) of type quadword. */
class Moffs64 : public Moffs {
	public:
		constexpr Moffs64(const Sreg* seg, const Imm64* offset) 
				: Moffs{seg, offset} { 
		}
		constexpr Moffs64(const Imm64* offset) 
				: Moffs{offset} { 
		}
		virtual constexpr OpType type() {
			return OpType::MOFFS_64;
		}
};

} // namespace x64asm

#endif
