#ifndef X64_SRC_OPERANDS_M_H
#define X64_SRC_OPERANDS_M_H

#include "src/operands/r.h"
#include "src/operands/imm.h"
#include "src/operands/operand.h"
#include "src/operands/scale.h"
#include "src/operands/sreg.h"

namespace x64 {

/** An operand in memory. */
class M : public Operand {
	public:
		inline M(uint64_t val) : Operand{val} { }

		inline M(R64 b)
				: Operand(concat(0x8, b.val_, 0x10, Scale::TIMES_1, 0, 0)) {
		}

		inline M(Sreg s, R64 b)
				: Operand(concat(s.val_, b.val_, 0x10, Scale::TIMES_1, 0, 0)) {
		}

		inline M(R64 b, Imm32 d)
				: Operand(concat(0x8, b.val_, 0x10, Scale::TIMES_1, d.val_, 0)) {
		}

		inline M(Sreg s, R64 b, Imm32 d)
				: Operand(concat(s.val_, b.val_, 0x10, Scale::TIMES_1, d.val_, 0)) {
		}

		inline M(R64 i, Scale sc)
				: Operand(concat(0x8, 0x10, i.val_, sc, 0, 0)) {
		}

		inline M(Sreg s, R64 i, Scale sc)
				: Operand(concat(s.val_, 0x10, i.val_, sc, 0, 0)) {
		}

		inline M(R64 i, Scale sc, Imm32 d) 
				: Operand(concat(0x8, 0x10, i.val_, sc, d.val_, 0)) {
		}

		inline M(Sreg s, R64 i, Scale sc, Imm32 d)
				: Operand(concat(s.val_, 0x10, i.val_, sc, d.val_, 0)) {
		}

		inline M(R64 b, R64 i, Scale sc)
				: Operand(concat(0x8, b.val_, i.val_, sc, 0, 0)) {
		}

		inline M(Sreg s, R64 b, R64 i, Scale sc)
				: Operand(concat(s.val_, b.val_, i.val_, sc, 0, 0)) {
		}

		inline M(R64 b, R64 i, Scale sc, Imm32 d)
				: Operand(concat(0x8, b.val_, i.val_, sc, d.val_, 0)) {
		}

		inline M(Sreg s, R64 b, R64 i, Scale sc, Imm32 d)
				: Operand(concat(s.val_, b.val_, i.val_, sc, d.val_, 0)) {
		}

		inline M(R32 b)
				: Operand(concat(0x8, b.val_, 0x10, Scale::TIMES_1, 0, 1)) {
		}

		inline M(Sreg s, R32 b)
				: Operand(concat(s.val_, b.val_, 0x10, Scale::TIMES_1, 0, 1)) {
		}

		inline M(R32 b, Imm32 d)
				: Operand(concat(0x8, b.val_, 0x10, Scale::TIMES_1, d.val_, 1)) {
		}

		inline M(Sreg s, R32 b, Imm32 d)
				: Operand(concat(s.val_, b.val_, 0x10, Scale::TIMES_1, d.val_, 1)) {
		}

		inline M(R32 i, Scale sc)
				: Operand(concat(0x8, 0x10, i.val_, sc, 0, 1)) {
		}

		inline M(Sreg s, R32 i, Scale sc) 
				: Operand(concat(s.val_, 0x10, i.val_, sc, 0, 1)) {
		}

		inline M(R32 i, Scale sc, Imm32 d)
				: Operand(concat(0x8, 0x10, i.val_, sc, d.val_, 1)) {
		}

		inline M(Sreg s, R32 i, Scale sc, Imm32 d)
				: Operand(concat(s.val_, 0x10, i.val_, sc, d.val_, 1)) {
		}

		inline M(R32 b, R32 i, Scale sc)
				: Operand(concat(0x8, b.val_, i.val_, sc, 0, 1)) {
		}

		inline M(Sreg s, R32 b, R32 i, Scale sc)
				: Operand(concat(s.val_, b.val_, i.val_, sc, 0, 1)) {
		}

		inline M(R32 b, R32 i, Scale sc, Imm32 d)
				: Operand(concat(0x8, b.val_, i.val_, sc, d.val_, 1)) {
		}

		inline M(Sreg s, R32 b, R32 i, Scale sc, Imm32 d)
				: Operand(concat(s.val_, b.val_, i.val_, sc, d.val_, 1)) {
		}

		inline bool null_seg() const {
			return val_ & (0x1ull << 47);
		}

		inline bool null_base() const {
			return val_ & (0x1ull << 43);
		}

		inline bool null_index() const {
			return val_ & (0x1ull << 38);
		}

		inline Sreg get_seg() const {
			return (val_ >> 44) & 0x7;
		}

		inline R get_base() const {
			return (val_ >> 39) & 0xf;
		}

		inline R get_index() const {
			return (val_ >> 34) & 0xf;
		}

		inline Scale get_scale() const {
			return (Scale) ((val_ >> 32) & 0x3);
		}

		inline Imm32 get_disp() const {
			return val_ & 0xffffffff;
		}

		inline bool get_addr_or() const {
			return (val_ >> 48) & 0x1;
		}

		inline void set_seg(Sreg s) {
			val_ &= ~(0xfull << 44);
			val_ |= (s.val_ & 0x7) << 45;
		}

		inline void clear_seg() {
			val_ |= (0x1ull << 47);
		}

		inline void set_base(R b) {
			val_ &= ~(0x1full << 39);
			val_ |= (b.val_ & 0xf) << 39;
		}

		inline void clear_base() {
			val_ |= (0x1ull << 43);
		}

		inline void set_index(R i) {
			val_ &= ~(0x1full << 34);
			val_ |= (i.val_ & 0xf) << 34;
		}

		inline void clear_index() {
			val_ |= (0x1ull << 38);
		}

		inline void set_scale(Scale s) {
			val_ &= ~(0x3ull << 32);
			val_ |= ((uint64_t) s & 0x3ull) << 32;
		}

		inline void set_disp(Imm32 d) {
			val_ &= 0xffffffff00000000;
			val_ |= (d.val_ & 0xffffffff);
		}

		inline void set_addr_or() {
			val_ |= (0x1ull << 48);
		}

		inline void clear_addr_or() {
			val_ &= ~(0x1ull << 48);
		}
			
		// addr (1=32)     [48]
		// segment? (1=no) [47]
		// segment         [46:44]
		// base? (1=no)    [43]
		// base            [42:39]
		// index? (1=no)   [38]
		// index           [37:34]
		// scale           [33:32]
		// displacement    [31:0]

		inline uint64_t concat(uint64_t s, uint64_t b, uint64_t i, Scale sc, 
				                   uint64_t d, uint64_t ao) {
			return d | (s << 32) | (i << 34) | (b << 39) | ((uint64_t) sc << 44) | 
				     (ao << 48);
		}
};

// NOTE: This ugliness can be replaced using inherited constructors come gcc 4.8
#define CONSTRUCTORS(T) \
	inline T(uint64_t val) : M{val} { } \
	inline T(R64 b) : M{b} { } \
	inline T(Sreg s, R64 b) : M{s, b} { } \
	inline T(R64 b, Imm32 d) : M{b, d} { } \
	inline T(Sreg s, R64 b, Imm32 d) : M{s, b, d} { } \
	inline T(R64 i, Scale s) : M{i, s} { } \
	inline T(Sreg s, R64 i, Scale sc) : M{s, i, sc} { } \
	inline T(R64 i, Scale s, Imm32 d) : M{i, s, d} { } \
	inline T(Sreg s, R64 i, Scale sc, Imm32 d) : M{s, i, sc, d} { } \
	inline T(R64 b, R64 i, Scale s) : M{b, i, s} { } \
	inline T(Sreg s, R64 b, R64 i, Scale sc) : M{s, b, i, sc} { } \
	inline T(R64 b, R64 i, Scale s, Imm32 d) : M{b, i, s, d} { } \
	inline T(Sreg s, R64 b, R64 i, Scale sc, Imm32 d) : M{s, b, i, sc, d} { } \
	inline T(R32 b) : M{b} { } \
	inline T(Sreg s, R32 b) : M{s, b} { } \
	inline T(R32 b, Imm32 d) : M{b, d} { } \
	inline T(Sreg s, R32 b, Imm32 d) : M{s, b, d} { } \
	inline T(R32 i, Scale s) : M{i, s} { } \
	inline T(Sreg s, R32 i, Scale sc) : M{s, i, sc} { } \
	inline T(R32 i, Scale s, Imm32 d) : M{i, s, d} { } \
	inline T(Sreg s, R32 i, Scale sc, Imm32 d) : M{s, i, sc, d} { } \
	inline T(R32 b, R32 i, Scale s) : M{b, i, s} { } \
	inline T(Sreg s, R32 b, R32 i, Scale sc) : M{s, b, i, sc} { } \
	inline T(R32 b, R32 i, Scale s, Imm32 d) : M{b, i, s, d} { } \
	inline T(Sreg s, R32 b, R32 i, Scale sc, Imm32 d) : M{s, b, i, sc, d} { } \

/** A byte operand in memory, usually expressed as a variable or array name, 
	  but pointed to by the DS:(E)SI or ES:(E)DI registers. 
		In 64-bit mode, it is pointed to by the RSI or RDI registers.
*/		
class M8 : public M {
	public:
		CONSTRUCTORS(M8)
};

/** A word operand in memory, usually expressed as a variable or array name, 
	  but pointed to by the DS:(E)SI or ES:(E)DI registers. This nomenclature is 
		used only with the string instructions.
*/
class M16 : public M {
	public:
		CONSTRUCTORS(M16)
};

/** A doubleword operand in memory, usually expressed as a variable or array 
		name, but pointed to by the DS:(E)SI or ES:(E)DI registers. This 
		nomenclature is used only with the string instructions.
*/
class M32 : public M {
	public:
		CONSTRUCTORS(M32)
};

/** A memory quadword operand in memory. */
class M64 : public M {
	public:
		CONSTRUCTORS(M64)
};

/** A memory double quadword operand in memory. */
class M128 : public M {
	public:
		CONSTRUCTORS(M128)
};

/** A 32-byte operand in memory. This nomenclature is used only with AVX 
	  instructions.
*/		
class M256 : public M {
	public:
		CONSTRUCTORS(M256)
};

/** A memory operand consisting of data item pairs whose sizes are indicated on 
	  the left and the right side of the ampersand. All memory addressing modes 
		are allowed. The m16&64 operand is used by LIDT and LGDT in 64-bit mode to 
		provide a word with which to load the limit field, and a quadword with 
		which to load the base field of the corresponding GDTR and IDTR registers.
*/
class MPair1664 : public M {
	public:
		CONSTRUCTORS(MPair1664)
};

/** A memory operand containing a far pointer composed of two numbers. The
		number to the left of the colon corresponds to the pointer's segment 
		selector. The number to the right corresponds to its offset.
*/
class MPtr1616 : public M {
	public:
		CONSTRUCTORS(MPtr1616)
};

/** A memory operand containing a far pointer composed of two numbers. The
		number to the left of the colon corresponds to the pointer's segment 
		selector. The number to the right corresponds to its offset.
*/
class MPtr1632 : public M {
	public:
		CONSTRUCTORS(MPtr1632)
};

/** A memory operand containing a far pointer composed of two numbers. The
		number to the left of the colon corresponds to the pointer's segment 
		selector. The number to the right corresponds to its offset.
*/
class MPtr1664 : public M {
	public:
		CONSTRUCTORS(MPtr1664)
};

/** A word integer operand in memory. This symbol designates integers that are 
	  used as operands for x87 FPU integer instructions.
*/
class M16Int : public M {
	public:
		CONSTRUCTORS(M16Int)
};

/** A doubleword integer operand in memory. This symbol designates integers 
	  that are used as operands for x87 FPU integer instructions.
*/
class M32Int : public M {
	public:
		CONSTRUCTORS(M32Int)
};

/** A quadword integer operand in memory. This symbol designates integers 
	  that are used as operands for x87 FPU integer instructions.
*/
class M64Int : public M {
	public:
		CONSTRUCTORS(M64Int)
};

/** A single-precision floating-point operand in memory. This symbol designates 
		floating-point values that are used as operands for x87 FPU floating-point 
		instructions.
*/
class M32Fp : public M {
	public:
		CONSTRUCTORS(M32Fp)
};

/** A double-precision floating-point operand in memory. This symbol designates 
		floating-point values that are used as operands for x87 FPU floating-point 
		instructions.
*/
class M64Fp : public M {
	public:
		CONSTRUCTORS(M64Fp)
};

/** A double extended-precision floating-point operand in memory. This symbol 
		designates floating-point values that are used as operands for x87 FPU 
		floating-point instructions.
*/
class M80Fp : public M {
	public:
		CONSTRUCTORS(M80Fp)
};

/** A double extended-precision binary-coded-decimaly operand in memory. */
class M80Bcd : public M {
	public:
		CONSTRUCTORS(M80Bcd)
};

/** A 2 byte operand in memory. */
class M2Byte : public M {
	public:
		CONSTRUCTORS(M2Byte)
};

/** A 14 byte operand in memory. */
class M14Byte : public M {
	public:
		CONSTRUCTORS(M14Byte)
};

/** A 28 byte operand in memory. */
class M28Byte : public M {
	public:
		CONSTRUCTORS(M28Byte)
};

/** A 94 byte operand in memory. */
class M94Byte : public M {
	public:
		CONSTRUCTORS(M94Byte)
};

/** A 108 byte operand in memory. */
class M108Byte : public M {
	public:
		CONSTRUCTORS(M108Byte)
};

/** A 5122 byte operand in memory. */
class M512Byte : public M {
	public:
		CONSTRUCTORS(M512Byte)
};

#undef CONSTRUCTORS

} // namespace x64

#endif
