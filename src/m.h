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

#ifndef X64ASM_SRC_M_H
#define X64ASM_SRC_M_H

#include <cassert>
#include <iostream>

#include "src/constants.h"
#include "src/r.h"
#include "src/imm.h"
#include "src/op_type.h"
#include "src/operand.h"
#include "src/sreg.h"

namespace x64asm {

/** Index register scaling constant. */
enum class Scale {
	TIMES_1 = 0,
	TIMES_2,
	TIMES_4,
	TIMES_8
};

/** An operand in memory. */
class M : public CompoundOperand {
	public:
		bool contains_seg() const {
			return seg_ != 0;
		}

		bool contains_base() const {
			return base_ != 0;
		}

		bool contains_index() const {
			return index_ != 0;
		}

		bool contains_disp() const {
			return disp_ != 0;
		}

		const Sreg* get_seg() const {
			assert(contains_seg());
			return seg_;
		}

		const AddrR* get_base() const {
			assert(contains_base());
			return base_;
		}

		const AddrR* get_index() const {
			assert(contains_index());
			return index_;
		}

		Scale get_scale() const {
			return scale_;
		}

		const Imm32* get_disp() const {
			assert(contains_disp());
			return disp_;
		}

		bool get_addr_or() const {
			return addr_or_;
		}

		void set_set(const Sreg* seg) {
			seg_ = seg;
		}

		void set_base(const AddrR* base) {
			base_ = base;
		}

		void set_index(const AddrR* index) {
			index_ = index;
		}

		void set_scale(Scale scale) {
			scale_ = scale;
		}

		void set_disp(const Imm32* disp) {
			disp_ = disp;
		}

		void set_addr_or(bool addr_or) {
			addr_or_ = addr_or;
		}

		void clear_seg() {
			seg_ = 0;
		}

		void clear_base() {
			base_ = 0;
		}

		void clear_index() {
			index_ = 0;
		}

		void clear_disp() {
			disp_ = 0;
		}

		virtual bool check() const;
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;

	protected:
		constexpr M(const Imm32* d, bool addr_or = false)
				: CompoundOperand{}, seg_{0}, base_{0}, index_{0},
			    scale_{Scale::TIMES_1}, disp_{d}, addr_or_{addr_or} {
		}

		constexpr M(const Sreg* s, const Imm32* d, bool addr_or = false)
				: CompoundOperand{}, seg_{s}, base_{0}, index_{0},
					scale_{Scale::TIMES_1}, disp_{d}, addr_or_{addr_or} {
		}

		constexpr M(const AddrR* b, bool addr_or = false)
				: CompoundOperand{}, seg_(0), base_(b), index_(0), 
			    scale_(Scale::TIMES_1), disp_(0), addr_or_(addr_or) {
		}

		constexpr M(const Sreg* s, const AddrR* b, bool addr_or = false)
				: CompoundOperand{}, seg_(s), base_(b), index_(0), 
			    scale_(Scale::TIMES_1), disp_(0), addr_or_(addr_or) {
		}

		constexpr M(const AddrR* b, const Imm32* d, bool addr_or = false)
				: CompoundOperand{}, seg_(0), base_(b), index_(0), 
			    scale_(Scale::TIMES_1), disp_(d), addr_or_(addr_or) {
		}

		constexpr M(const Sreg* s, const AddrR* b, const Imm32* d, 
				     bool addr_or = false)
				: CompoundOperand{}, seg_(s), base_(b), index_(0), 
			    scale_(Scale::TIMES_1), disp_(d), addr_or_(addr_or) {
		}

		constexpr M(const AddrR* i, Scale sc, bool addr_or = false)
				: CompoundOperand{}, seg_(0), base_(0), index_(i), scale_(sc), 
			    disp_(0), addr_or_(addr_or) {
		}

		constexpr M(const Sreg* s, const AddrR* i, Scale sc, bool addr_or = false)
				: CompoundOperand{}, seg_(s), base_(0), index_(i), scale_(sc), 
			    disp_(0), addr_or_(addr_or) {
		}

		constexpr M(const AddrR* i, Scale sc, const Imm32* d, bool addr_or = false) 
				: CompoundOperand{}, seg_(0), base_(0), index_(i), scale_(sc), 
			    disp_(d), addr_or_(addr_or) {
		}

		constexpr M(const Sreg* s, const AddrR* i, Scale sc, const Imm32* d, 
				     bool addr_or = false)
				: CompoundOperand{}, seg_(s), base_(0), index_(i), scale_(sc), 
			    disp_(d), addr_or_(addr_or) {
		}

		constexpr M(const AddrR* b, const AddrR* i, Scale sc, bool addr_or = false)
				: CompoundOperand{}, seg_(0), base_(b), index_(i), scale_(sc), 
			    disp_(0), addr_or_(addr_or) {
		}

		constexpr M(const Sreg* s, const AddrR* b, const AddrR* i, Scale sc, 
				     bool addr_or = false)
				: CompoundOperand{}, seg_(s), base_(b), index_(i), scale_(sc), 
			    disp_(0), addr_or_(addr_or) {
		}

		constexpr M(const AddrR* b, const AddrR* i, Scale sc, const Imm32* d, 
				     bool addr_or = false)
				: CompoundOperand{}, seg_(0), base_(b), index_(i), scale_(sc), 
			    disp_(d), addr_or_(addr_or) {
		}

		constexpr M(const Sreg* s, const AddrR* b, const AddrR* i, Scale sc, 
				     const Imm32* d, bool addr_or = false)
				: CompoundOperand{}, seg_(s), base_(b), index_(i), scale_(sc), 
			    disp_(d), addr_or_(addr_or) {
		}

		virtual void write_intel_width(std::ostream& os) const;

	private:
		virtual void insert_in(RegSet& os, bool promote = false) const;

		const Sreg* seg_;
		const AddrR* base_;
		const AddrR* index_;
		Scale scale_;
		const Imm32* disp_;
		bool addr_or_;
};

// NOTE: This ugliness can be replaced using inherited constructors come gcc 4.8
#define CONSTRUCTORS(T) \
	constexpr T(const Imm32* d, bool o = false) : M{d, o} { } \
	constexpr T(const Sreg* s, const Imm32* d, bool o = false) : M{s, d, o} { } \
	constexpr T(const AddrR* b, bool o = false) : M{b, o} { } \
	constexpr T(const Sreg* s, const AddrR* b, bool o = false) : M{s, b, o} { } \
	constexpr T(const AddrR* b, const Imm32* d, bool o = false) : M{b, d, o} { } \
	constexpr T(const Sreg* s, const AddrR* b, const Imm32* d, bool o = false) : M{s, b, d, o} { } \
	constexpr T(const AddrR* i, Scale s, bool o = false) : M{i, s, o} { } \
	constexpr T(const Sreg* s, const AddrR* i, Scale sc, bool o = false) : M{s, i, sc, o} { } \
	constexpr T(const AddrR* i, Scale s, const Imm32* d, bool o = false) : M{i, s, d, o} { } \
	constexpr T(const Sreg* s, const AddrR* i, Scale sc, const Imm32* d, bool o = false) : M{s, i, sc, d, o} { } \
	constexpr T(const AddrR* b, const AddrR* i, Scale s, bool o = false) : M{b, i, s, o} { } \
	constexpr T(const Sreg* s, const AddrR* b, const AddrR* i, Scale sc, bool o = false) : M{s, b, i, sc, o} { } \
	constexpr T(const AddrR* b, const AddrR* i, Scale s, const Imm32* d, bool o = false) : M{b, i, s, d, o} { } \
	constexpr T(const Sreg* s, const AddrR* b, const AddrR* i, Scale sc, const Imm32* d, bool o = false) : M{s, b, i, sc, d, o} { } \

/** A byte operand in memory, usually expressed as a variable or array name, 
	  but pointed to by the DS:(E)SI or ES:(E)DI registers. 
		In 64-bit mode, it is pointed to by the RSI or RDI registers.
*/		
class M8 : public M {
	public:
		CONSTRUCTORS(M8)
	protected:
		virtual void write_intel_width(std::ostream& os) const;
	private:
		virtual constexpr OpType type() {
			return OpType::M_8;
		}
};

/** A word operand in memory, usually expressed as a variable or array name, 
	  but pointed to by the DS:(E)SI or ES:(E)DI registers. This nomenclature is 
		used only with the string instructions.
*/
class M16 : public M {
	public:
		CONSTRUCTORS(M16)
	protected:
		virtual void write_intel_width(std::ostream& os) const;
	private:
		virtual constexpr OpType type() {
			return OpType::M_16;
		}
};

/** A doubleword operand in memory, usually expressed as a variable or array 
		name, but pointed to by the DS:(E)SI or ES:(E)DI registers. This 
		nomenclature is used only with the string instructions.
*/
class M32 : public M {
	public:
		CONSTRUCTORS(M32)
	protected:
		virtual void write_intel_width(std::ostream& os) const;
	private:
		virtual constexpr OpType type() {
			return OpType::M_32;
		}
};

/** A memory quadword operand in memory. */
class M64 : public M {
	public:
		CONSTRUCTORS(M64)
	protected:
		virtual void write_intel_width(std::ostream& os) const;
	private:
		virtual constexpr OpType type() {
			return OpType::M_64;
		}
};

/** A memory double quadword operand in memory. */
class M128 : public M {
	public:
		CONSTRUCTORS(M128)
	protected:
		virtual void write_intel_width(std::ostream& os) const;
	private:
		virtual constexpr OpType type() {
			return OpType::M_128;
		}
};

/** A 32-byte operand in memory. This nomenclature is used only with AVX 
	  instructions.
*/		
class M256 : public M {
	public:
		CONSTRUCTORS(M256)
	protected:
		virtual void write_intel_width(std::ostream& os) const;
	private:
		virtual constexpr OpType type() {
			return OpType::M_256;
		}
};

/** A word integer operand in memory. This symbol designates integers that are 
	  used as operands for x87 FPU integer instructions.
*/
class M16Int : public M {
	public:
		CONSTRUCTORS(M16Int)
	protected:
		virtual void write_intel_width(std::ostream& os) const;
	private:
		virtual constexpr OpType type() {
			return OpType::M_16_INT;
		}
};

/** A doubleword integer operand in memory. This symbol designates integers 
	  that are used as operands for x87 FPU integer instructions.
*/
class M32Int : public M {
	public:
		CONSTRUCTORS(M32Int)
	protected:
		virtual void write_intel_width(std::ostream& os) const;
	private:
		virtual constexpr OpType type() {
			return OpType::M_32_INT;
		}
};

/** A quadword integer operand in memory. This symbol designates integers 
	  that are used as operands for x87 FPU integer instructions.
*/
class M64Int : public M {
	public:
		CONSTRUCTORS(M64Int)
	protected:
		virtual void write_intel_width(std::ostream& os) const;
	private:
		virtual constexpr OpType type() {
			return OpType::M_64_INT;
		}
};

/** A single-precision floating-point operand in memory. This symbol designates 
		floating-point values that are used as operands for x87 FPU floating-point 
		instructions.
*/
class M32Fp : public M {
	public:
		CONSTRUCTORS(M32Fp)
	protected:
		virtual void write_intel_width(std::ostream& os) const;
	private:
		virtual constexpr OpType type() {
			return OpType::M_32_FP;
		}
};

/** A double-precision floating-point operand in memory. This symbol designates 
		floating-point values that are used as operands for x87 FPU floating-point 
		instructions.
*/
class M64Fp : public M {
	public:
		CONSTRUCTORS(M64Fp)
	protected:
		virtual void write_intel_width(std::ostream& os) const;
	private:
		virtual constexpr OpType type() {
			return OpType::M_64_FP;
		}
};

/** A double extended-precision floating-point operand in memory. This symbol 
		designates floating-point values that are used as operands for x87 FPU 
		floating-point instructions.
*/
class M80Fp : public M {
	public:
		CONSTRUCTORS(M80Fp)
	protected:
		virtual void write_intel_width(std::ostream& os) const;
	private:
		virtual constexpr OpType type() {
			return OpType::M_80_FP;
		}
};

/** A double extended-precision binary-coded-decimaly operand in memory. */
class M80Bcd : public M {
	public:
		CONSTRUCTORS(M80Bcd)
	protected:
		virtual void write_intel_width(std::ostream& os) const;
	private:
		virtual constexpr OpType type() {
			return OpType::M_80_BCD;
		}
};

/** A 2 byte operand in memory. */
class M2Byte : public M {
	public:
		CONSTRUCTORS(M2Byte)
	protected:
		virtual void write_intel_width(std::ostream& os) const;
	private:
		virtual constexpr OpType type() {
			return OpType::M_2_BYTE;
		}
};

/** A 14 byte operand in memory. */
class M14Byte : public M {
	public:
		CONSTRUCTORS(M14Byte)
	protected:
		virtual void write_intel_width(std::ostream& os) const;
	private:
		virtual constexpr OpType type() {
			return OpType::M_14_BYTE;
		}
};

/** A 28 byte operand in memory. */
class M28Byte : public M {
	public:
		CONSTRUCTORS(M28Byte)
	protected:
		virtual void write_intel_width(std::ostream& os) const;
	private:
		virtual constexpr OpType type() {
			return OpType::M_28_BYTE;
		}
};

/** A 94 byte operand in memory. */
class M94Byte : public M {
	public:
		CONSTRUCTORS(M94Byte)
	protected:
		virtual void write_intel_width(std::ostream& os) const;
	private:
		virtual constexpr OpType type() {
			return OpType::M_94_BYTE;
		}
};

/** A 108 byte operand in memory. */
class M108Byte : public M {
	public:
		CONSTRUCTORS(M108Byte)
	protected:
		virtual void write_intel_width(std::ostream& os) const;
	private:
		virtual constexpr OpType type() {
			return OpType::M_108_BYTE;
		}
};

/** A 5122 byte operand in memory. */
class M512Byte : public M {
	public:
		CONSTRUCTORS(M512Byte)
	protected:
		virtual void write_intel_width(std::ostream& os) const;
	private:
		virtual constexpr OpType type() {
			return OpType::M_512_BYTE;
		}
};

/** A memory operand containing a far pointer composed of two numbers. The
		number to the left of the colon corresponds to the pointer's segment 
		selector. The number to the right corresponds to its offset.
*/
class FarPtr1616 : public M {
	public:
		CONSTRUCTORS(FarPtr1616)
	protected:
		virtual void write_intel_width(std::ostream& os) const;
	private:
		virtual constexpr OpType type() {
			return OpType::FAR_PTR_16_16;
		}
};

/** A memory operand containing a far pointer composed of two numbers. The
		number to the left of the colon corresponds to the pointer's segment 
		selector. The number to the right corresponds to its offset.
*/
class FarPtr1632 : public M {
	public:
		CONSTRUCTORS(FarPtr1632)
	protected:
		virtual void write_intel_width(std::ostream& os) const;
	private:
		virtual constexpr OpType type() {
			return OpType::FAR_PTR_16_32;
		}
};

/** A memory operand containing a far pointer composed of two numbers. The
		number to the left of the colon corresponds to the pointer's segment 
		selector. The number to the right corresponds to its offset.
*/
class FarPtr1664 : public M {
	public:
		CONSTRUCTORS(FarPtr1664)
	protected:
		virtual void write_intel_width(std::ostream& os) const;
	private:
		virtual constexpr OpType type() {
			return OpType::FAR_PTR_16_64;
		}
};

#undef CONSTRUCTORS

} // namespace x64asm

#endif
