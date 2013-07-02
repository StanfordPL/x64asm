/*
Copyright 2013 eric schkufza

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
    /** Constant bit masks used to represent absent operands. */
    enum class Null : uint64_t {
      BASE  = 0x10,
      INDEX = 0x10,
      SEG   = 0x7
    };

    /** Bit mask representing where operand are stored in underlying value. */
    enum class Mask : uint64_t {
      DISP    = 0x00000000ffffffff,
      BASE    = 0x0000001f00000000,
      INDEX   = 0x00001f0000000000,
      SCALE   = 0x0003000000000000,
      SEG     = 0x0700000000000000,
      ADDR_OR = 0x1000000000000000,
      RIP     = 0x2000000000000000
    };

    /** Index of operand in underlying bit mask. */
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
    /** Returns true if this memory contains a segment register. */
    constexpr bool contains_seg() {
      return (val_ & (uint64_t)Mask::SEG) !=
             ((uint64_t)Null::SEG << (uint64_t)Index::SEG);
    }

    /** Returns true if this memory contains a base register. */
    constexpr bool contains_base() {
      return (val_ & (uint64_t)Mask::BASE) !=
             ((uint64_t)Null::BASE << (uint64_t)Index::BASE);
    }

    /** Returns true if this memory contains an index register. */
    constexpr bool contains_index() {
      return (val_ & (uint64_t)Mask::INDEX) !=
             ((uint64_t)Null::INDEX << (uint64_t)Index::INDEX);
    }

    /** Returns this memory's segment register; undefined if absent. */
    constexpr Sreg get_seg() {
      return Sreg {(val_ & (uint64_t)Mask::SEG) >> (uint64_t)Index::SEG};
    }

    /** Returns this memory's base register; undefined if absent. */
    constexpr R64 get_base() {
      return R64 {(val_ & (uint64_t)Mask::BASE) >> (uint64_t)Index::BASE};
    }

    /** Returns this memory's index register; undefined if absent. */
    constexpr R64 get_index() {
      return R64 {(val_ & (uint64_t)Mask::INDEX) >> (uint64_t)Index::INDEX};
    }

    /** Returns this memory's index scaling constant; 1 if absent. */
    constexpr Scale get_scale() {
      return (Scale)((val_ & (uint64_t)Mask::SCALE) >> (uint64_t)Index::SCALE);
    }

    /** Returns this memory's displacement; 0 if absent. */
    constexpr Imm32 get_disp() {
      return Imm32 {(uint32_t)(val_ & (uint64_t)Mask::DISP)};
    }

    /** Returns true if this memory uses a 32-bit address override. */
    constexpr bool get_addr_or()  {
      return val_ & (uint64_t)Mask::ADDR_OR;
    }

    /** Returns true if this memory uses RIP+offset form.  This implies that
    	  contains_base() and contains_index() are all false.
    */
    constexpr bool rip_offset() {
      return val_ & (uint64_t)Mask::RIP;
    }

    /** Sets this memory's segment register. */
    void set_seg(const Sreg& seg) {
      val_ &= ~(uint64_t)Mask::SEG;
      val_ |= seg.val_ << (uint64_t)Index::SEG;
    }

    /** Sets this memory's base register. */
    void set_base(const R& base) {
      val_ &= ~(uint64_t)Mask::BASE;
      val_ |= base.val_ << (uint64_t)Index::BASE;
    }

    /** Sets this memory's index register. */
    void set_index(const R& index) {
      val_ &= ~(uint64_t)Mask::INDEX;
      val_ |= index.val_ << (uint64_t)Index::INDEX;
    }

    /** Sets this memory's scale register. */
    void set_scale(Scale scale) {
      val_ &= ~(uint64_t)Mask::SCALE;
      val_ |= (uint64_t)scale << (uint64_t)Index::SCALE;
    }

    /** Sets this memory's displacement. */
    void set_disp(const Imm32& disp) {
      val_ &= ~(uint64_t)Mask::DISP;
      val_ |= (uint64_t)disp.val_ << (uint64_t)Index::DISP;
    }

    /** Sets the 32-bit address override bit for this memory. */
    void set_addr_or(bool addr_or) {
      if (addr_or) {
        val_ |= (uint64_t)Mask::ADDR_OR;
      } else {
        val_ &= ~(uint64_t)Mask::ADDR_OR;
      }
    }

    /** Sets the RIP+offset form flag for this memory. */
    void set_rip_offset(bool rip) {
      if (rip) {
        val_ |= (uint64_t)Mask::RIP;
      } else {
        val_ &= ~(uint64_t)Mask::RIP;
      }
    }

    /** Removes the segment register from this memory. */
    void clear_seg() {
      set_seg(Sreg {(uint64_t)Null::SEG});
    }

    /** Removes the base register from this memory. */
    void clear_base() {
      set_seg(Sreg {(uint64_t)Null::BASE});
    }

    /** Remvoes the index register from this memory. */
    void clear_index() {
      set_seg(Sreg {(uint64_t)Null::INDEX});
    }

    /** Returns true if this memory is well-formed: all present registers are
    	  well formed, the index register is not rsp, and that the RIP+offset() bit
    		is set only if base and index registers are absent.
    */
    bool check() const;

    /** Comparison based on underlying value. */
    constexpr bool operator<(const M& rhs) {
      return val_ < rhs.val_;
    }

    /** Comparison based on underlying value. */
    constexpr bool operator==(const M& rhs) {
      return val_ == rhs.val_;
    }

    /** Writes this memory to an ostream using at&t syntax. */
    void write_att(std::ostream& os) const;

  protected:
    /** Helper method: initializes all internal fields. */
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

    /** Creates a memory using disp form. */
    constexpr M(const Imm32& d)
      : Operand {init(d.val_, (uint64_t)Null::BASE, (uint64_t)Null::INDEX,
                      (uint64_t)Scale::TIMES_1, (uint64_t)Null::SEG, 0, 0)
    } {
    }

    /** Creates a memory using seg:disp form. */
    constexpr M(const Sreg& s, const Imm32& d)
      : Operand {init(d.val_, (uint64_t)Null::BASE, (uint64_t)Null::INDEX,
                      (uint64_t)Scale::TIMES_1, s.val_, 0, 0)
    } {
    }

    /** Creates a memroy using (base64) form. */
    constexpr M(const R32& b)
      : Operand {init(0, b.val_, (uint64_t)Null::INDEX,
                      (uint64_t)Scale::TIMES_1, (uint64_t)Null::SEG, 1, 0)
    } {
    }

    /** Creates a memory using (base32) form. */
    constexpr M(const R64& b)
      : Operand {init(0, b.val_, (uint64_t)Null::INDEX,
                      (uint64_t)Scale::TIMES_1, (uint64_t)Null::SEG, 0, 0)
    } {
    }

    /** Creates a memory using RIP form. */
    constexpr M(Rip rip)
      : Operand {init(0, (uint64_t)Null::BASE, (uint64_t)Null::INDEX,
                      (uint64_t)Scale::TIMES_1, (uint64_t)Null::SEG, 0, 1)
    } {
    }

    /** Creates a memory using seg:base32 form. */
    constexpr M(const Sreg& s, const R32& b)
      : Operand {init(0, b.val_, (uint64_t)Null::INDEX,
                      (uint64_t)Scale::TIMES_1, s.val_, 1, 0)
    } {
    }

    /** Creates a memory using seg:base64 form. */
    constexpr M(const Sreg& s, const R64& b)
      : Operand {init(0, b.val_, (uint64_t)Null::INDEX,
                      (uint64_t)Scale::TIMES_1, s.val_, 0, 0)
    } {
    }

    /** Creates a memory using seg:RIP form. */
    constexpr M(const Sreg& s, Rip rip)
      : Operand {init(0, (uint64_t)Null::BASE, (uint64_t)Null::INDEX,
                      (uint64_t)Scale::TIMES_1, s.val_, 0, 1)
    } {
    }

    /** Creates a memory using disp(base32) form. */
    constexpr M(const R32& b, const Imm32& d)
      : Operand {init(d.val_, b.val_, (uint64_t)Null::INDEX,
                      (uint64_t)Scale::TIMES_1, (uint64_t)Null::SEG, 1, 0)
    } {
    }

    /** Creates a memory using disp(base64) form. */
    constexpr M(const R64& b, const Imm32& d)
      : Operand {init(d.val_, b.val_, (uint64_t)Null::INDEX,
                      (uint64_t)Scale::TIMES_1, (uint64_t)Null::SEG, 0, 0)
    } {
    }

    /** Creates a memory using RIP+disp form. */
    constexpr M(Rip rip, const Imm32& d)
      : Operand {init(d.val_, (uint64_t)Null::BASE, (uint64_t)Null::INDEX,
                      (uint64_t)Scale::TIMES_1, (uint64_t)Null::SEG, 0, 1)
    } {
    }

    /** Creates a memory using seg:disp(base32) form. */
    constexpr M(const Sreg& s, const R32& b, const Imm32& d)
      : Operand {init(d.val_, b.val_, (uint64_t)Null::INDEX,
                      (uint64_t)Scale::TIMES_1, s.val_, 1, 0)
    } {
    }

    /** Creates a memory using seg:disp(base64) form. */
    constexpr M(const Sreg& s, const R64& b, const Imm32& d)
      : Operand {init(d.val_, b.val_, (uint64_t)Null::INDEX,
                      (uint64_t)Scale::TIMES_1, s.val_, 0, 0)
    } {
    }

    /** Creates a memory using seg:RIP+disp form. */
    constexpr M(const Sreg& s, Rip rip, const Imm32& d)
      : Operand {init(d.val_, (uint64_t)Null::BASE, (uint64_t)Null::INDEX,
                      (uint64_t)Scale::TIMES_1, s.val_, 0, 1)
    } {
    }

    /** Creates a memory using (index32,scale) form. */
    constexpr M(const R32& i, Scale sc)
      : Operand {init(0, (uint64_t)Null::BASE, i.val_, (uint64_t)sc,
                      (uint64_t)Null::SEG, 1, 0)
    } {
    }

    /** Creates a memory using (index64,scale) form. */
    constexpr M(const R64& i, Scale sc)
      : Operand {init(0, (uint64_t)Null::BASE, i.val_, (uint64_t)sc,
                      (uint64_t)Null::SEG, 0, 0)
    } {
    }

    /** Creates a memory using seg:(index32,scale) form. */
    constexpr M(const Sreg& s, const R32& i, Scale sc)
      : Operand {init(0, (uint64_t)Null::BASE, i.val_, (uint64_t)sc, s.val_, 1, 0)} {
    }

    /** Creates a memory using seg:(index64,scale) form. */
    constexpr M(const Sreg& s, const R64& i, Scale sc)
      : Operand {init(0, (uint64_t)Null::BASE, i.val_, (uint64_t)sc, s.val_, 0, 0)} {
    }

    /** Creates a memory using disp(index32,scale) form. */
    constexpr M(const R32& i, Scale sc, const Imm32& d)
      : Operand {init(d.val_, (uint64_t)Null::BASE, i.val_, (uint64_t)sc,
                      (uint64_t)Null::SEG, 1, 0)
    } {
    }

    /** Creates a memory using disp(index64,scale) form. */
    constexpr M(const R64& i, Scale sc, const Imm32& d)
      : Operand {init(d.val_, (uint64_t)Null::BASE, i.val_, (uint64_t)sc,
                      (uint64_t)Null::SEG, 0, 0)
    } {
    }

    /** Creates a memory using seg:disp(index32,scale) form. */
    constexpr M(const Sreg& s, const R32& i, Scale sc, const Imm32& d)
      : Operand {init(d.val_, (uint64_t)Null::BASE, i.val_, (uint64_t)sc, s.val_, 1, 0)} {
    }

    /** Creates a memory using seg:disp(index64,scale) form. */
    constexpr M(const Sreg& s, const R64& i, Scale sc, const Imm32& d)
      : Operand {init(d.val_, (uint64_t)Null::BASE, i.val_, (uint64_t)sc, s.val_, 0, 0)} {
    }

    /** Creates a memory using (base32,index32,scale) form. */
    constexpr M(const R32& b, const R32& i, Scale sc)
      : Operand {init(0, b.val_, i.val_, (uint64_t)sc, (uint64_t)Null::SEG, 1, 0)} {
    }

    /** Creates a memory using (base64,index64,scale) form. */
    constexpr M(const R64& b, const R64& i, Scale sc)
      : Operand {init(0, b.val_, i.val_, (uint64_t)sc, (uint64_t)Null::SEG, 0, 0)} {
    }

    /** Creates a memory using seg:(base32,index32,scale) form. */
    constexpr M(const Sreg& s, const R32& b, const R32& i, Scale sc)
      : Operand {init(0, b.val_, i.val_, (uint64_t)sc, s.val_, 1, 0)} {
    }

    /** Creates a memory using seg:(base64,index64,scale) form. */
    constexpr M(const Sreg& s, const R64& b, const R64& i, Scale sc)
      : Operand {init(0, b.val_, i.val_, (uint64_t)sc, s.val_, 0, 0)} {
    }

    /** Creates a memory using disp(base32,index32,scale) form. */
    constexpr M(const R32& b, const R32& i, Scale sc, const Imm32& d)
      : Operand {init(d.val_, b.val_, i.val_, (uint64_t)sc, (uint64_t)Null::SEG, 1, 0)} {
    }

    /** Creates a memory using disp(base64,index64,scale) form. */
    constexpr M(const R64& b, const R64& i, Scale sc, const Imm32& d)
      : Operand {init(d.val_, b.val_, i.val_, (uint64_t)sc, (uint64_t)Null::SEG, 0, 0)} {
    }

    /** Creates a memory using seg:disp(base32,index32,scale) form. */
    constexpr M(const Sreg& s, const R32& b, const R32& i, Scale sc,
                const Imm32& d)
      : Operand {init(d.val_, b.val_, i.val_, (uint64_t)sc, s.val_, 1, 0)} {
    }

    /** Creates a memory using seg:disp(base64,index64,scale) form. */
    constexpr M(const Sreg& s, const R64& b, const R64& i, Scale sc,
                const Imm32& d)
      : Operand {init(d.val_, b.val_, i.val_, (uint64_t)sc, s.val_, 0, 0)} {
    }
};

// This ugliness can be replaced using inherited constructors come gcc 4.8
#define CONSTRUCTORS(T) \
	/** Creates a memory using disp form. */ \
	constexpr T(const Imm32& d) : M{d} { } \
	/** Creates a memory using seg:disp form. */ \
	constexpr T(const Sreg& s, const Imm32& d) : M{s, d} { } \
	/** Creates a memroy using (base64) form. */ \
	constexpr T(const R32& b) : M{b} { } \
	/** Creates a memory using (base32) form. */ \
	constexpr T(const R64& b) : M{b} { } \
	/** Creates a memory using RIP form. */ \
	constexpr T(Rip rip) : M{rip} {} \
	/** Creates a memory using seg:base32 form. */ \
	constexpr T(const Sreg& s, const R32& b) : M{s, b} { } \
	/** Creates a memory using seg:base64 form. */ \
	constexpr T(const Sreg& s, const R64& b) : M{s, b} { } \
	/** Creates a memory using seg:RIP form. */ \
	constexpr T(const Sreg& s, Rip rip) : M{s, rip} { } \
	/** Creates a memory using disp(base32) form. */ \
	constexpr T(const R32& b, const Imm32& d) : M{b, d} { } \
	/** Creates a memory using disp(base64) form. */ \
	constexpr T(const R64& b, const Imm32& d) : M{b, d} { } \
	/** Creates a memory using RIP+disp form. */ \
	constexpr T(Rip rip, const Imm32& d) : M{rip, d} { } \
	/** Creates a memory using seg:disp(base32) form. */ \
	constexpr T(const Sreg& s, const R32& b, const Imm32& d) : M{s, b, d} { } \
	/** Creates a memory using seg:disp(base64) form. */ \
	constexpr T(const Sreg& s, const R64& b, const Imm32& d) : M{s, b, d} { } \
	/** Creates a memory using seg:RIP+disp form. */ \
	constexpr T(const Sreg& s, Rip rip, const Imm32& d) : M{s, rip, d} { } \
	/** Creates a memory using (index32,scale) form. */ \
	constexpr T(const R32& i, Scale s) : M{i, s} { } \
	/** Creates a memory using (index64,scale) form. */ \
	constexpr T(const R64& i, Scale s) : M{i, s} { } \
	/** Creates a memory using seg:(index32,scale) form. */ \
	constexpr T(const Sreg& s, const R32& i, Scale sc) : M{s, i, sc} { } \
	/** Creates a memory using seg:(index64,scale) form. */ \
	constexpr T(const Sreg& s, const R64& i, Scale sc) : M{s, i, sc} { } \
	/** Creates a memory using disp(index32,scale) form. */ \
	constexpr T(const R32& i, Scale s, const Imm32& d) : M{i, s, d} { } \
	/** Creates a memory using disp(index64,scale) form. */ \
	constexpr T(const R64& i, Scale s, const Imm32& d) : M{i, s, d} { } \
	/** Creates a memory using seg:disp(index32,scale) form. */ \
	constexpr T(const Sreg& s, const R32& i, Scale sc, const Imm32& d) : M{s, i, sc, d} { } \
	/** Creates a memory using seg:disp(index64,scale) form. */ \
	constexpr T(const Sreg& s, const R64& i, Scale sc, const Imm32& d) : M{s, i, sc, d} { } \
	/** Creates a memory using (base32,index32,scale) form. */ \
	constexpr T(const R32& b, const R32& i, Scale s) : M{b, i, s} { } \
	/** Creates a memory using (base64,index64,scale) form. */ \
	constexpr T(const R64& b, const R64& i, Scale s) : M{b, i, s} { } \
	/** Creates a memory using seg:(base32,index32,scale) form. */ \
	constexpr T(const Sreg& s, const R32& b, const R32& i, Scale sc) : M{s, b, i, sc} { } \
	/** Creates a memory using seg:(base64,index64,scale) form. */ \
	constexpr T(const Sreg& s, const R64& b, const R64& i, Scale sc) : M{s, b, i, sc} { } \
	/** Creates a memory using disp(base32,index32,scale) form. */ \
	constexpr T(const R32& b, const R32& i, Scale s, const Imm32& d) : M{b, i, s, d} { } \
	/** Creates a memory using disp(base64,index64,scale) form. */ \
	constexpr T(const R64& b, const R64& i, Scale s, const Imm32& d) : M{b, i, s, d} { } \
	/** Creates a memory using seg:disp(base32,index32,scale) form. */ \
	constexpr T(const Sreg& s, const R32& b, const R32& i, Scale sc, const Imm32& d) : M{s, b, i, sc, d} { } \
	/** Creates a memory using seg:disp(base64,index64,scale) form. */ \
	constexpr T(const Sreg& s, const R64& b, const R64& i, Scale sc, const Imm32& d) : M{s, b, i, sc, d} { } \
 
/** A byte operand in memory, usually expressed as a variable or array name,
	  but pointed to by the DS:(E)SI or ES:(E)DI registers.
		In 64-bit mode, it is pointed to by the RSI or RDI registers.
*/
class M8 : public M {
  public:
    CONSTRUCTORS(M8);
};

/** A word operand in memory, usually expressed as a variable or array name,
	  but pointed to by the DS:(E)SI or ES:(E)DI registers. This nomenclature is
		used only with the string instructions.
*/
class M16 : public M {
  public:
    CONSTRUCTORS(M16);
};

/** A doubleword operand in memory, usually expressed as a variable or array
		name, but pointed to by the DS:(E)SI or ES:(E)DI registers. This
		nomenclature is used only with the string instructions.
*/
class M32 : public M {
  public:
    CONSTRUCTORS(M32);
};

/** A memory quadword operand in memory. */
class M64 : public M {
  public:
    CONSTRUCTORS(M64);
};

/** A memory double quadword operand in memory. */
class M128 : public M {
  public:
    CONSTRUCTORS(M128);
};

/** A 32-byte operand in memory. This nomenclature is used only with AVX
	  instructions.
*/
class M256 : public M {
  public:
    CONSTRUCTORS(M256);
};

/** A word integer operand in memory. This symbol designates integers that are
	  used as operands for x87 FPU integer instructions.
*/
class M16Int : public M {
  public:
    CONSTRUCTORS(M16Int);
};

/** A doubleword integer operand in memory. This symbol designates integers
	  that are used as operands for x87 FPU integer instructions.
*/
class M32Int : public M {
  public:
    CONSTRUCTORS(M32Int);
};

/** A quadword integer operand in memory. This symbol designates integers
	  that are used as operands for x87 FPU integer instructions.
*/
class M64Int : public M {
  public:
    CONSTRUCTORS(M64Int);
};

/** A single-precision floating-point operand in memory. This symbol designates
		floating-point values that are used as operands for x87 FPU floating-point
		instructions.
*/
class M32Fp : public M {
  public:
    CONSTRUCTORS(M32Fp);
};

/** A double-precision floating-point operand in memory. This symbol designates
		floating-point values that are used as operands for x87 FPU floating-point
		instructions.
*/
class M64Fp : public M {
  public:
    CONSTRUCTORS(M64Fp);
};

/** A double extended-precision floating-point operand in memory. This symbol
		designates floating-point values that are used as operands for x87 FPU
		floating-point instructions.
*/
class M80Fp : public M {
  public:
    CONSTRUCTORS(M80Fp);
};

/** A double extended-precision binary-coded-decimaly operand in memory. */
class M80Bcd : public M {
  public:
    CONSTRUCTORS(M80Bcd);
};

/** A 2 byte operand in memory. */
class M2Byte : public M {
  public:
    CONSTRUCTORS(M2Byte);
};

/** A 28 byte operand in memory. */
class M28Byte : public M {
  public:
    CONSTRUCTORS(M28Byte);
};

/** A 108 byte operand in memory. */
class M108Byte : public M {
  public:
    CONSTRUCTORS(M108Byte);
};

/** A 512 byte operand in memory. */
class M512Byte : public M {
  public:
    CONSTRUCTORS(M512Byte);
};

/** A memory operand containing a far pointer composed of two numbers. The
		number to the left of the colon corresponds to the pointer's segment
		selector. The number to the right corresponds to its offset.
*/
class FarPtr1616 : public M {
  public:
    CONSTRUCTORS(FarPtr1616);
};

/** A memory operand containing a far pointer composed of two numbers. The
		number to the left of the colon corresponds to the pointer's segment
		selector. The number to the right corresponds to its offset.
*/
class FarPtr1632 : public M {
  public:
    CONSTRUCTORS(FarPtr1632);
};

/** A memory operand containing a far pointer composed of two numbers. The
		number to the left of the colon corresponds to the pointer's segment
		selector. The number to the right corresponds to its offset.
*/
class FarPtr1664 : public M {
  public:
    CONSTRUCTORS(FarPtr1664);
};

#undef CONSTRUCTORS

} // namespace x64asm

#endif
