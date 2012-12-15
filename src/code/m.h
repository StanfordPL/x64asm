#ifndef X64_SRC_CODE_M_H
#define X64_SRC_CODE_M_H

#include "src/code/r.h"
#include "src/code/imm.h"
#include "src/code/operand.h"
#include "src/code/sreg.h"

namespace x64 {

	/*  NULL CONDITION
		inline bool is_null() const {
			return (get_base().is_null() && get_scale().is_null()) || 
				      get_index() == rsp ||
							get_scale() == scale_null;			     
		}
	*/

/** An operand in memory. */
class M {
	private:
		enum Null {
			NULL_R = 0x10, NULL_SREG = 0x8
		};

	public:
		enum Scale {
			TIMES_1 = 0, TIMES_2,	TIMES_4, TIMES_8
		};

		inline M(R64 b) {
			set_all(NULL_SREG, b, NULL_R, TIMES_1, 0, 0);
		}

		inline M(Sreg s, R64 b) {
			set_all(s, b, NULL_R, TIMES_1, 0, 0);
		}

		inline M(R64 b, Imm32 d) {
			set_all(NULL_SREG, b, NULL_R, TIMES_1, d, 0);
		}

		inline M(Sreg s, R64 b, Imm32 d) {
			set_all(s, b, NULL_R, TIMES_1, d, 0);
		}

		inline M(R64 i, Scale s) {
			set_all(NULL_SREG, NULL_R, i, s, 0, 0);
		}

		inline M(Sreg s, R64 i, Scale sc) {
			set_all(s, NULL_R, i, sc, 0, 0);
		}

		inline M(R64 i, Scale s, Imm32 d) {
			set_all(NULL_SREG, NULL_R, i, s, d, 0);
		}

		inline M(Sreg s, R64 i, Scale sc, Imm32 d) {
			set_all(s, NULL_R, i, sc, d, 0);
		}

		inline M(R64 b, R64 i, Scale s) {
			set_all(NULL_SREG, b, i, s, 0, 0);
		}

		inline M(Sreg s, R64 b, R64 i, Scale sc) {
			set_all(s, b, i, sc, 0, 0);
		}

		inline M(R64 b, R64 i, Scale s, Imm32 d) {
			set_all(NULL_SREG, b, i, s, d, 0);
		}

		inline M(Sreg s, R64 b, R64 i, Scale sc, Imm32 d) {
			set_all(s, b, i, sc, d, 0);
		}

		inline M(R32 b) {
			set_all(NULL_SREG, b, NULL_R, TIMES_1, 0, 1);
		}

		inline M(Sreg s, R32 b) {
			set_all(s, b, NULL_R, TIMES_1, 0, 1);
		}

		inline M(R32 b, Imm32 d) {
			set_all(NULL_SREG, b, NULL_R, TIMES_1, d, 1);
		}

		inline M(Sreg s, R32 b, Imm32 d) {
			set_all(s, b, NULL_R, TIMES_1, d, 1);
		}

		inline M(R32 i, Scale s) {
			set_all(NULL_SREG, NULL_R, i, s, 0, 1);
		}

		inline M(Sreg s, R32 i, Scale sc) {
			set_all(s, NULL_R, i, sc, 0, 1);
		}

		inline M(R32 i, Scale s, Imm32 d) {
			set_all(NULL_SREG, NULL_R, i, s, d, 1);
		}

		inline M(Sreg s, R32 i, Scale sc, Imm32 d) {
			set_all(s, NULL_R, i, sc, d, 1);
		}

		inline M(R32 b, R32 i, Scale s) {
			set_all(NULL_SREG, b, i, s, 0, 1);
		}

		inline M(Sreg s, R32 b, R32 i, Scale sc) {
			set_all(s, b, i, sc, 0, 1);
		}

		inline M(R32 b, R32 i, Scale s, Imm32 d) {
			set_all(NULL_SREG, b, i, s, d, 1);
		}

		inline M(Sreg s, R32 b, R32 i, Scale sc, Imm32 d) {
			set_all(s, b, i, sc, d, 1);
		}

		inline M(Operand m) 
				: m_(m) {
		}

		inline operator Operand() const {
			return m_;
		}

		inline bool null_seg() const {
			return m_ & (0x1ull << 47);
		}

		inline bool null_base() const {
			return m_ & (0x1ull << 43);
		}

		inline bool null_index() const {
			return m_ & (0x1ull << 38);
		}

		inline Sreg get_seg() const {
			return (m_ >> 44) & 0x7;
		}

		inline R get_base() const {
			return (m_ >> 39) & 0xf;
		}

		inline R get_index() const {
			return (m_ >> 34) & 0xf;
		}

		inline Scale get_scale() const {
			return (Scale) ((m_ >> 32) & 0x3);
		}

		inline Imm32 get_disp() const {
			return m_ & 0xffffffff;
		}

		inline bool get_addr_or() const {
			return (m_ >> 48) & 0x1;
		}

		inline void set_seg(Sreg s) {
			m_ &= ~(0xfull << 44);
			m_ |= (s & 0x7) << 45;
		}

		inline void clear_seg() {
			m_ |= (0x1ull << 47);
		}

		inline void set_base(R b) {
			m_ &= ~(0x1full << 39);
			m_ |= (b & 0xf) << 39;
		}

		inline void clear_base() {
			m_ |= (0x1ull << 43);
		}

		inline void set_index(R i) {
			m_ &= ~(0x1full << 34);
			m_ |= (i & 0xf) << 34;
		}

		inline void clear_index() {
			m_ |= (0x1ull << 38);
		}

		inline void set_scale(Scale s) {
			m_ &= ~(0x3ull << 32);
			m_ |= (s & 0x3ull) << 32;
		}

		inline void set_disp(Imm32 d) {
			m_ &= 0xffffffff00000000;
			m_ |= (d & 0xffffffff);
		}

		inline void set_addr_or() {
			m_ |= (0x1ull << 48);
		}

		inline void clear_addr_or() {
			m_ &= ~(0x1ull << 48);
		}
			
	private:
		Operand m_;

		// addr (1=32)     [48]
		// segment? (1=no) [47]
		// segment         [46:44]
		// base? (1=no)    [43]
		// base            [42:39]
		// index? (1=no)   [38]
		// index           [37:34]
		// scale           [33:32]
		// displacement    [31:0]

		inline void set_all(Operand s, Operand b, Operand i, Operand sc, 
				                Operand d, Operand ao) {
			m_ = d | (sc << 32) | (i << 34) | (b << 39) | (s << 44) | (ao << 48);
		}
};

// NOTE: This ugliness can be replaced using inherited constructors come gcc 4.8
#define CONSTRUCTORS(T) \
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
	inline T(Operand o) : M{o} { } 

/** A byte operand in memory, usually expressed as a variable or array name, 
	  but pointed to by the DS:(E)SI or ES:(E)DI registers. 
		In 64-bit mode, it is pointed to by the RSI or RDI registers.
*/		
struct M8 : public M {
	CONSTRUCTORS(M8)
};

/** A word operand in memory, usually expressed as a variable or array name, 
	  but pointed to by the DS:(E)SI or ES:(E)DI registers. This nomenclature is 
		used only with the string instructions.
*/
struct M16 : public M {
	CONSTRUCTORS(M16)
};

/** A doubleword operand in memory, usually expressed as a variable or array 
		name, but pointed to by the DS:(E)SI or ES:(E)DI registers. This 
		nomenclature is used only with the string instructions.
*/
struct M32 : public M {
	CONSTRUCTORS(M32)
};

/** A memory quadword operand in memory. */
struct M64 : public M {
	CONSTRUCTORS(M64)
};

/** A memory double quadword operand in memory. */
struct M128 : public M {
	CONSTRUCTORS(M128)
};

/** A 32-byte operand in memory. This nomenclature is used only with AVX 
	  instructions.
*/		
struct M256 : public M {
	CONSTRUCTORS(M256)
};

/** A memory operand consisting of data item pairs whose sizes are indicated on 
	  the left and the right side of the ampersand. All memory addressing modes 
		are allowed. The m16&64 operand is used by LIDT and LGDT in 64-bit mode to 
		provide a word with which to load the limit field, and a quadword with 
		which to load the base field of the corresponding GDTR and IDTR registers.
*/
struct MPair1664 : public M {
	CONSTRUCTORS(MPair1664)
};

/** A memory operand containing a far pointer composed of two numbers. The
		number to the left of the colon corresponds to the pointer's segment 
		selector. The number to the right corresponds to its offset.
*/
struct MPtr1616 : public M {
	CONSTRUCTORS(MPtr1616)
};

/** A memory operand containing a far pointer composed of two numbers. The
		number to the left of the colon corresponds to the pointer's segment 
		selector. The number to the right corresponds to its offset.
*/
struct MPtr1632 : public M {
	CONSTRUCTORS(MPtr1632)
};

/** A memory operand containing a far pointer composed of two numbers. The
		number to the left of the colon corresponds to the pointer's segment 
		selector. The number to the right corresponds to its offset.
*/
struct MPtr1664 : public M {
	CONSTRUCTORS(MPtr1664)
};

/** A word integer operand in memory. This symbol designates integers that are 
	  used as operands for x87 FPU integer instructions.
*/
struct M16Int : public M {
	CONSTRUCTORS(M16Int)
};

/** A doubleword integer operand in memory. This symbol designates integers 
	  that are used as operands for x87 FPU integer instructions.
*/
struct M32Int : public M {
	CONSTRUCTORS(M32Int)
};

/** A quadword integer operand in memory. This symbol designates integers 
	  that are used as operands for x87 FPU integer instructions.
*/
struct M64Int : public M {
	CONSTRUCTORS(M64Int)
};

/** A single-precision floating-point operand in memory. This symbol designates 
		floating-point values that are used as operands for x87 FPU floating-point 
		instructions.
*/
struct M32Fp : public M {
	CONSTRUCTORS(M32Fp)
};

/** A double-precision floating-point operand in memory. This symbol designates 
		floating-point values that are used as operands for x87 FPU floating-point 
		instructions.
*/
struct M64Fp : public M {
	CONSTRUCTORS(M64Fp)
};

/** A double extended-precision floating-point operand in memory. This symbol 
		designates floating-point values that are used as operands for x87 FPU 
		floating-point instructions.
*/
struct M80Fp : public M {
	CONSTRUCTORS(M80Fp)
};

/** A double extended-precision binary-coded-decimaly operand in memory. */
struct M80Bcd : public M {
	CONSTRUCTORS(M80Bcd)
};

/** A 2 byte operand in memory. */
struct M2Byte : public M {
	CONSTRUCTORS(M2Byte)
};

/** A 14 byte operand in memory. */
struct M14Byte : public M {
	CONSTRUCTORS(M14Byte)
};

/** A 28 byte operand in memory. */
struct M28Byte : public M {
	CONSTRUCTORS(M28Byte)
};

/** A 94 byte operand in memory. */
struct M94Byte : public M {
	CONSTRUCTORS(M94Byte)
};

/** A 108 byte operand in memory. */
struct M108Byte : public M {
	CONSTRUCTORS(M108Byte)
};

/** A 5122 byte operand in memory. */
struct M512Byte : public M {
	CONSTRUCTORS(M512Byte)
};

#undef CONSTRUCTORS

} // namespace x64

#endif
