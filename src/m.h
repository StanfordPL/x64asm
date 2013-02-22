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
#include "src/env_reg.h"
#include "src/r.h"
#include "src/imm.h"
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
class M : public Operand {
	private:
		enum class Null : uint64_t {
			BASE  = 0x10,
			INDEX = 0x10,
			SEG   = 0x7
		};

		enum class Mask : uint64_t {
			DISP    = 0x00000000ffffffff,
			BASE    = 0x0000001f00000000,
			INDEX   = 0x00001f0000000000,
			SCALE   = 0x0003000000000000,
			SEG     = 0x0700000000000000,
			ADDR_OR = 0x1000000000000000,
			RIP     = 0x2000000000000000
		};

		enum class Index {
			DISP    = 0,
			BASE    = 32,
			INDEX   = 40,
			SCALE   = 48,
			SEG     = 56,
			ADDR_OR = 60,
			RIP     = 61
		};

	public:
		constexpr bool contains_seg() {
			return (val_ & (uint64_t)Mask::SEG) != 
				     ((uint64_t)Null::SEG << (uint64_t)Index::SEG);
		}

		constexpr bool contains_base() {
			return (val_ & (uint64_t)Mask::BASE) != 
				     ((uint64_t)Null::BASE << (uint64_t)Index::BASE);
		}

		constexpr bool contains_index() {
			return (val_ & (uint64_t)Mask::INDEX) != 
				     ((uint64_t)Null::INDEX << (uint64_t)Index::INDEX);
		}

		constexpr Sreg get_seg() {
			return Sreg{(val_ & (uint64_t)Mask::SEG) >> (uint64_t)Index::SEG};
		}

		constexpr R32 get_base() {
			return R32{(val_ & (uint64_t)Mask::BASE) >> (uint64_t)Index::BASE};
		}

		constexpr R32 get_index() {
			return R32{(val_ & (uint64_t)Mask::INDEX) >> (uint64_t)Index::INDEX};
		}

		constexpr Scale get_scale() {
			return (Scale)((val_ & (uint64_t)Mask::SCALE) >> (uint64_t)Index::SCALE);
		}

		constexpr Imm32 get_disp() {
			return Imm32{(uint32_t)(val_ & (uint64_t)Mask::DISP)};
		}

		constexpr bool get_addr_or()  {
			return val_ & (uint64_t)Mask::ADDR_OR;
		}

		constexpr bool rip_offset() {
			return val_ & (uint64_t)Mask::RIP;
		}

		void set_seg(const Sreg& seg) {
			val_ &= ~(uint64_t)Mask::SEG;
			val_ |= seg.val_ << (uint64_t)Index::SEG;
		}

		void set_base(const R& base) {
			val_ &= ~(uint64_t)Mask::BASE;
			val_ |= base.val_ << (uint64_t)Index::BASE;
		}

		void set_index(const R& index) {
			val_ &= ~(uint64_t)Mask::INDEX;
			val_ |= index.val_ << (uint64_t)Index::INDEX;
		}

		void set_scale(Scale scale) {
			val_ &= ~(uint64_t)Mask::SCALE;
			val_ |= (uint64_t)scale << (uint64_t)Index::SCALE;
		}

		void set_disp(const Imm32& disp) {
			val_ &= ~(uint64_t)Mask::DISP;
			val_ |= (uint64_t)disp.val_ << (uint64_t)Index::DISP;
		}

		void set_addr_or(bool addr_or) {
			if ( addr_or )
				val_ |= (uint64_t)Mask::ADDR_OR;
			else
				val_ &= ~(uint64_t)Mask::ADDR_OR;
		}

		void set_rip_offset(bool rip) {
			if ( rip )
				val_ |= (uint64_t)Mask::RIP;
			else
				val_ &= ~(uint64_t)Mask::RIP;
		}

		void clear_seg() {
			set_seg(Sreg{(uint64_t)Null::SEG});
		}

		void clear_base() {
			set_seg(Sreg{(uint64_t)Null::BASE});
		}

		void clear_index() {
			set_seg(Sreg{(uint64_t)Null::INDEX});
		}

		bool check() const;

		constexpr bool operator<(const M& rhs) {
			return val_ == rhs.val_ ? val2_ < rhs.val2_ : val_ < rhs.val_;
		}

		constexpr bool operator==(const M& rhs) {
			return val_ == rhs.val_ && val2_ == rhs.val2_;
		}

		void write_att(std::ostream& os) const;

	protected:
		static constexpr 
		uint64_t init(uint64_t d, uint64_t b, uint64_t i, uint64_t sc, uint64_t s, 
				          uint64_t addr_or, uint64_t rip) {
			return (d & (uint64_t)Mask::DISP) |
				     (b << (uint64_t)Index::BASE) |
						 (i << (uint64_t)Index::INDEX) |
						 ((uint64_t)sc << (uint64_t)Index::SCALE) |
						 (s << (uint64_t)Index::SEG) |
						 (addr_or << (uint64_t)Index::ADDR_OR) |
						 (rip << (uint64_t)Index::RIP);
		}

		constexpr M(const Imm32& d)
				: Operand{init(d.val_, (uint64_t)Null::BASE, (uint64_t)Null::INDEX, 
						           (uint64_t)Scale::TIMES_1, (uint64_t)Null::SEG, 0, 0)} {
		}

		constexpr M(const Sreg& s, const Imm32& d)
				: Operand{init(d.val_, (uint64_t)Null::BASE, (uint64_t)Null::INDEX, 
						           (uint64_t)Scale::TIMES_1, s.val_, 0, 0)} {
		}

		constexpr M(const R32& b)
				: Operand{init(0, b.val_, (uint64_t)Null::INDEX, 
						           (uint64_t)Scale::TIMES_1, (uint64_t)Null::SEG, 1, 0)} {
		}

		constexpr M(const R64& b)
				: Operand{init(0, b.val_, (uint64_t)Null::INDEX, 
						           (uint64_t)Scale::TIMES_1, (uint64_t)Null::SEG, 0, 0)} {
		}

		constexpr M(Rip rip)
				: Operand{init(0, (uint64_t)Null::BASE, (uint64_t)Null::INDEX, 
						           (uint64_t)Scale::TIMES_1, (uint64_t)Null::SEG, 0, 1)} {
		}

		constexpr M(const Sreg& s, const R32& b)
				: Operand{init(0, b.val_, (uint64_t)Null::INDEX, 
						           (uint64_t)Scale::TIMES_1, s.val_, 1, 0)} {
		}

		constexpr M(const Sreg& s, const R64& b)
				: Operand{init(0, b.val_, (uint64_t)Null::INDEX, 
						           (uint64_t)Scale::TIMES_1, s.val_, 0, 0)} {
		}

		constexpr M(const Sreg& s, Rip rip)
				: Operand{init(0, (uint64_t)Null::BASE, (uint64_t)Null::INDEX, 
						           (uint64_t)Scale::TIMES_1, s.val_, 0, 1)} {
		}

		constexpr M(const R32& b, const Imm32& d)
				: Operand{init(d.val_, b.val_, (uint64_t)Null::INDEX, 
						           (uint64_t)Scale::TIMES_1, (uint64_t)Null::SEG, 1, 0)} {
		}

		constexpr M(const R64& b, const Imm32& d)
				: Operand{init(d.val_, b.val_, (uint64_t)Null::INDEX, 
						           (uint64_t)Scale::TIMES_1, (uint64_t)Null::SEG, 0, 0)} {
		}

		constexpr M(Rip rip, const Imm32& d)
				: Operand{init(d.val_, (uint64_t)Null::BASE, (uint64_t)Null::INDEX, 
						           (uint64_t)Scale::TIMES_1, (uint64_t)Null::SEG, 0, 1)} {
		}

		constexpr M(const Sreg& s, const R32& b, const Imm32& d)
				: Operand{init(d.val_, b.val_, (uint64_t)Null::INDEX, 
						           (uint64_t)Scale::TIMES_1, s.val_, 1, 0)} {
		}

		constexpr M(const Sreg& s, const R64& b, const Imm32& d)
				: Operand{init(d.val_, b.val_, (uint64_t)Null::INDEX, 
						           (uint64_t)Scale::TIMES_1, s.val_, 0, 0)} {
		}

		constexpr M(const Sreg& s, Rip rip, const Imm32& d)
				: Operand{init(d.val_, (uint64_t)Null::BASE, (uint64_t)Null::INDEX, 
						           (uint64_t)Scale::TIMES_1, s.val_, 0, 1)} {
		}

		constexpr M(const R32& i, Scale sc)
				: Operand{init(0, (uint64_t)Null::BASE, i.val_, (uint64_t)sc, 
						           (uint64_t)Null::SEG, 1, 0)} {
		}

		constexpr M(const R64& i, Scale sc)
				: Operand{init(0, (uint64_t)Null::BASE, i.val_, (uint64_t)sc, 
						           (uint64_t)Null::SEG, 0, 0)} {
		}

		constexpr M(const Sreg& s, const R32& i, Scale sc)
				: Operand{init(0, (uint64_t)Null::BASE, i.val_, (uint64_t)sc, s.val_, 1, 0)} {
		}

		constexpr M(const Sreg& s, const R64& i, Scale sc)
				: Operand{init(0, (uint64_t)Null::BASE, i.val_, (uint64_t)sc, s.val_, 0, 0)} {
		}

		constexpr M(const R32& i, Scale sc, const Imm32& d)
				: Operand{init(d.val_, (uint64_t)Null::BASE, i.val_, (uint64_t)sc, 
						           (uint64_t)Null::SEG, 1, 0)} {
		}

		constexpr M(const R64& i, Scale sc, const Imm32& d)
				: Operand{init(d.val_, (uint64_t)Null::BASE, i.val_, (uint64_t)sc, 
						           (uint64_t)Null::SEG, 0, 0)} {
		}

		constexpr M(const Sreg& s, const R32& i, Scale sc, const Imm32& d)
				: Operand{init(d.val_, (uint64_t)Null::BASE, i.val_, (uint64_t)sc, s.val_, 1, 0)} {
		}

		constexpr M(const Sreg& s, const R64& i, Scale sc, const Imm32& d)
				: Operand{init(d.val_, (uint64_t)Null::BASE, i.val_, (uint64_t)sc, s.val_, 0, 0)} {
		}

		constexpr M(const R32& b, const R32& i, Scale sc)
				: Operand{init(0, b.val_, i.val_, (uint64_t)sc, (uint64_t)Null::SEG, 1, 0)} {
		}

		constexpr M(const R64& b, const R64& i, Scale sc)
				: Operand{init(0, b.val_, i.val_, (uint64_t)sc, (uint64_t)Null::SEG, 0, 0)} {
		}

		constexpr M(const Sreg& s, const R32& b, const R32& i, Scale sc)
				: Operand{init(0, b.val_, i.val_, (uint64_t)sc, s.val_, 1, 0)} {
		}

		constexpr M(const Sreg& s, const R64& b, const R64& i, Scale sc) 
				: Operand{init(0, b.val_, i.val_, (uint64_t)sc, s.val_, 0, 0)} {
		}

		constexpr M(const R32& b, const R32& i, Scale sc, const Imm32& d)
				: Operand{init(d.val_, b.val_, i.val_, (uint64_t)sc, (uint64_t)Null::SEG, 1, 0)} {
		}

		constexpr M(const R64& b, const R64& i, Scale sc, const Imm32& d)
				: Operand{init(d.val_, b.val_, i.val_, (uint64_t)sc, (uint64_t)Null::SEG, 0, 0)} {
		}

		constexpr M(const Sreg& s, const R32& b, const R32& i, Scale sc, 
				        const Imm32& d)
				: Operand{init(d.val_, b.val_, i.val_, (uint64_t)sc, s.val_, 1, 0)} {
		}

		constexpr M(const Sreg& s, const R64& b, const R64& i, Scale sc, 
				        const Imm32& d)
				: Operand{init(d.val_, b.val_, i.val_, (uint64_t)sc, s.val_, 0, 0)} {
		}

		void write_intel_base(std::ostream& os) const;
};

// NOTE: This ugliness can be replaced using inherited constructors come gcc 4.8
#define CONSTRUCTORS(T) \
	constexpr T(const Imm32& d) : M{d} { } \
	constexpr T(const Sreg& s, const Imm32& d) : M{s, d} { } \
	constexpr T(const R32& b) : M{b} { } \
	constexpr T(const R64& b) : M{b} { } \
	constexpr T(Rip rip) : M{rip} {} \
	constexpr T(const Sreg& s, const R32& b) : M{s, b} { } \
	constexpr T(const Sreg& s, const R64& b) : M{s, b} { } \
	constexpr T(const Sreg& s, Rip rip) : M{s, rip} { } \
	constexpr T(const R32& b, const Imm32& d) : M{b, d} { } \
	constexpr T(const R64& b, const Imm32& d) : M{b, d} { } \
	constexpr T(Rip rip, const Imm32& d) : M{rip, d} { } \
	constexpr T(const Sreg& s, const R32& b, const Imm32& d) : M{s, b, d} { } \
	constexpr T(const Sreg& s, const R64& b, const Imm32& d) : M{s, b, d} { } \
	constexpr T(const Sreg& s, Rip rip, const Imm32& d) : M{s, rip, d} { } \
	constexpr T(const R32& i, Scale s) : M{i, s} { } \
	constexpr T(const R64& i, Scale s) : M{i, s} { } \
	constexpr T(const Sreg& s, const R32& i, Scale sc) : M{s, i, sc} { } \
	constexpr T(const Sreg& s, const R64& i, Scale sc) : M{s, i, sc} { } \
	constexpr T(const R32& i, Scale s, const Imm32& d) : M{i, s, d} { } \
	constexpr T(const R64& i, Scale s, const Imm32& d) : M{i, s, d} { } \
	constexpr T(const Sreg& s, const R32& i, Scale sc, const Imm32& d) : M{s, i, sc, d} { } \
	constexpr T(const Sreg& s, const R64& i, Scale sc, const Imm32& d) : M{s, i, sc, d} { } \
	constexpr T(const R32& b, const R32& i, Scale s) : M{b, i, s} { } \
	constexpr T(const R64& b, const R64& i, Scale s) : M{b, i, s} { } \
	constexpr T(const Sreg& s, const R32& b, const R32& i, Scale sc) : M{s, b, i, sc} { } \
	constexpr T(const Sreg& s, const R64& b, const R64& i, Scale sc) : M{s, b, i, sc} { } \
	constexpr T(const R32& b, const R32& i, Scale s, const Imm32& d) : M{b, i, s, d} { } \
	constexpr T(const R64& b, const R64& i, Scale s, const Imm32& d) : M{b, i, s, d} { } \
	constexpr T(const Sreg& s, const R32& b, const R32& i, Scale sc, const Imm32& d) : M{s, b, i, sc, d} { } \
	constexpr T(const Sreg& s, const R64& b, const R64& i, Scale sc, const Imm32& d) : M{s, b, i, sc, d} { } \

/** A byte operand in memory, usually expressed as a variable or array name, 
	  but pointed to by the DS:(E)SI or ES:(E)DI registers. 
		In 64-bit mode, it is pointed to by the RSI or RDI registers.
*/		
class M8 : public M {
	public:
		CONSTRUCTORS(M8);

		void write_intel(std::ostream& os) const;
};

/** A word operand in memory, usually expressed as a variable or array name, 
	  but pointed to by the DS:(E)SI or ES:(E)DI registers. This nomenclature is 
		used only with the string instructions.
*/
class M16 : public M {
	public:
		CONSTRUCTORS(M16);

		void write_intel(std::ostream& os) const;
};

/** A doubleword operand in memory, usually expressed as a variable or array 
		name, but pointed to by the DS:(E)SI or ES:(E)DI registers. This 
		nomenclature is used only with the string instructions.
*/
class M32 : public M {
	public:
		CONSTRUCTORS(M32);

		void write_intel(std::ostream& os) const;
};

/** A memory quadword operand in memory. */
class M64 : public M {
	public:
		CONSTRUCTORS(M64);

		void write_intel(std::ostream& os) const;
};

/** A memory double quadword operand in memory. */
class M128 : public M {
	public:
		CONSTRUCTORS(M128);

		void write_intel(std::ostream& os) const;
};

/** A 32-byte operand in memory. This nomenclature is used only with AVX 
	  instructions.
*/		
class M256 : public M {
	public:
		CONSTRUCTORS(M256);

		void write_intel(std::ostream& os) const;
};

/** A word integer operand in memory. This symbol designates integers that are 
	  used as operands for x87 FPU integer instructions.
*/
class M16Int : public M {
	public:
		CONSTRUCTORS(M16Int);

		void write_intel(std::ostream& os) const;
};

/** A doubleword integer operand in memory. This symbol designates integers 
	  that are used as operands for x87 FPU integer instructions.
*/
class M32Int : public M {
	public:
		CONSTRUCTORS(M32Int);

		void write_intel(std::ostream& os) const;
};

/** A quadword integer operand in memory. This symbol designates integers 
	  that are used as operands for x87 FPU integer instructions.
*/
class M64Int : public M {
	public:
		CONSTRUCTORS(M64Int);

		void write_intel(std::ostream& os) const;
};

/** A single-precision floating-point operand in memory. This symbol designates 
		floating-point values that are used as operands for x87 FPU floating-point 
		instructions.
*/
class M32Fp : public M {
	public:
		CONSTRUCTORS(M32Fp);

		void write_intel(std::ostream& os) const;
};

/** A double-precision floating-point operand in memory. This symbol designates 
		floating-point values that are used as operands for x87 FPU floating-point 
		instructions.
*/
class M64Fp : public M {
	public:
		CONSTRUCTORS(M64Fp);

		void write_intel(std::ostream& os) const;
};

/** A double extended-precision floating-point operand in memory. This symbol 
		designates floating-point values that are used as operands for x87 FPU 
		floating-point instructions.
*/
class M80Fp : public M {
	public:
		CONSTRUCTORS(M80Fp);

		void write_intel(std::ostream& os) const;
};

/** A double extended-precision binary-coded-decimaly operand in memory. */
class M80Bcd : public M {
	public:
		CONSTRUCTORS(M80Bcd);

		void write_intel(std::ostream& os) const;
};

/** A 2 byte operand in memory. */
class M2Byte : public M {
	public:
		CONSTRUCTORS(M2Byte);

		void write_intel(std::ostream& os) const;
};

/** A 28 byte operand in memory. */
class M28Byte : public M {
	public:
		CONSTRUCTORS(M28Byte);

		void write_intel(std::ostream& os) const;
};

/** A 108 byte operand in memory. */
class M108Byte : public M {
	public:
		CONSTRUCTORS(M108Byte);

		void write_intel(std::ostream& os) const;
};

/** A 512 byte operand in memory. */
class M512Byte : public M {
	public:
		CONSTRUCTORS(M512Byte);

		void write_intel(std::ostream& os) const;
};

/** A memory operand containing a far pointer composed of two numbers. The
		number to the left of the colon corresponds to the pointer's segment 
		selector. The number to the right corresponds to its offset.
*/
class FarPtr1616 : public M {
	public:
		CONSTRUCTORS(FarPtr1616);

		void write_intel(std::ostream& os) const;
};

/** A memory operand containing a far pointer composed of two numbers. The
		number to the left of the colon corresponds to the pointer's segment 
		selector. The number to the right corresponds to its offset.
*/
class FarPtr1632 : public M {
	public:
		CONSTRUCTORS(FarPtr1632);

		void write_intel(std::ostream& os) const;
};

/** A memory operand containing a far pointer composed of two numbers. The
		number to the left of the colon corresponds to the pointer's segment 
		selector. The number to the right corresponds to its offset.
*/
class FarPtr1664 : public M {
	public:
		CONSTRUCTORS(FarPtr1664);

		void write_intel(std::ostream& os) const;
};

#undef CONSTRUCTORS

} // namespace x64asm

#endif
