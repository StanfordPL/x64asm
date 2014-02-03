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
      REG  = 0x10,
      SEG  = 0x7
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
    /** Copy constructor. */
    M(const M& rhs);
    /** Move constructor. */
    M(M&& rhs);
    /** Copy assignment operator. */
    M& operator=(const M& rhs);
    /** Move assignment operator. */
    M& operator=(M&& rhs);

    /** Returns true if this memory contains a segment register. */
    constexpr bool contains_seg();
    /** Returns true if this memory contains a base register. */
    constexpr bool contains_base();
    /** Returns true if this memory contains an index register. */
    constexpr bool contains_index();

    /** Returns true if this memory uses a 32-bit address override. */
    constexpr bool addr_or();
    /** Returns true if this memory uses RIP+offset form. */
    constexpr bool rip_offset();

    /** Returns this memory's segment register; undefined if absent. */
    constexpr Sreg get_seg();
    /** Returns this memory's base register; undefined if absent. */
    constexpr R64 get_base();
    /** Returns this memory's index register; undefined if absent. */
    constexpr R64 get_index();
    /** Returns this memory's index scaling constant; 1 if absent. */
    constexpr Scale get_scale();
    /** Returns this memory's displacement; 0 if absent. */
    constexpr Imm32 get_disp();

    /** Sets this memory's segment register. */
    void set_seg(const Sreg& seg);
    /** Sets this memory's base register. */
    void set_base(const R& base);
    /** Sets this memory's index register. */
    void set_index(const R& index);
    /** Sets this memory's scale register. */
    void set_scale(Scale scale);
    /** Sets this memory's displacement. */
    void set_disp(const Imm32& disp);

    /** Sets the 32-bit address override bit for this memory. */
    void set_addr_or(bool addr_or);
    /** Sets the RIP+offset form flag for this memory. */
    void set_rip_offset(bool rip);

    /** Removes the segment register from this memory. */
    void clear_seg();
    /** Removes the base register from this memory. */
    void clear_base();
    /** Remvoes the index register from this memory. */
    void clear_index();

    /** Returns true if this memory is well-formed: all present registers are
      well formed, the index register is not rsp, and that the RIP+offset() bit
      is set only if base and index registers are absent.
    */
    bool check() const;

    /** Comparison based on on val_. */
    constexpr bool operator<(const M& rhs);
    /** Comparison based on on val_. */
    constexpr bool operator==(const M& rhs);
    /** Comparison based on on val_. */
    constexpr bool operator!=(const M& rhs);

    /** STL-compliant hash. */
    constexpr size_t hash();
    /** STL-compliant swap. */
    void swap(M& rhs);

    /** Writes this xmm register to an ostream using at&t syntax. */
    std::ostream& write_att(std::ostream& os) const;

  protected:
    /** Helper method: returns a null register. */
    static constexpr R64 r_null();
    /** Helper method: returns a null segment register. */
    static constexpr Sreg s_null();
    /** Helper method: initializes all internal fields. */
    static constexpr uint64_t init(const Imm32& d, const R& b, const R& i, 
        Scale sc, const Sreg& s, uint64_t addr_or, uint64_t rip);

	public:
    /** Creates a memory using disp form. */
    constexpr M(const Imm32& d);
    /** Creates a memory using seg:disp form. */
    constexpr M(const Sreg& s, const Imm32& d);
    /** Creates a memroy using (base64) form. */
    constexpr M(const R32& b);
    /** Creates a memory using (base32) form. */
    constexpr M(const R64& b);
    /** Creates a memory using RIP form. */
    constexpr M(Rip rip);
    /** Creates a memory using seg:base32 form. */
    constexpr M(const Sreg& s, const R32& b);
    /** Creates a memory using seg:base64 form. */
    constexpr M(const Sreg& s, const R64& b);
    /** Creates a memory using seg:RIP form. */
    constexpr M(const Sreg& s, Rip rip);
    /** Creates a memory using disp(base32) form. */
    constexpr M(const R32& b, const Imm32& d);
    /** Creates a memory using disp(base64) form. */
    constexpr M(const R64& b, const Imm32& d);
    /** Creates a memory using RIP+disp form. */
    constexpr M(Rip rip, const Imm32& d);
    /** Creates a memory using seg:disp(base32) form. */
    constexpr M(const Sreg& s, const R32& b, const Imm32& d);
    /** Creates a memory using seg:disp(base64) form. */
    constexpr M(const Sreg& s, const R64& b, const Imm32& d);
    /** Creates a memory using seg:RIP+disp form. */
    constexpr M(const Sreg& s, Rip rip, const Imm32& d);
    /** Creates a memory using (index32,scale) form. */
    constexpr M(const R32& i, Scale sc);
    /** Creates a memory using (index64,scale) form. */
    constexpr M(const R64& i, Scale sc);
    /** Creates a memory using seg:(index32,scale) form. */
    constexpr M(const Sreg& s, const R32& i, Scale sc);
    /** Creates a memory using seg:(index64,scale) form. */
    constexpr M(const Sreg& s, const R64& i, Scale sc);
    /** Creates a memory using disp(index32,scale) form. */
    constexpr M(const R32& i, Scale sc, const Imm32& d);
    /** Creates a memory using disp(index64,scale) form. */
    constexpr M(const R64& i, Scale sc, const Imm32& d);
    /** Creates a memory using seg:disp(index32,scale) form. */
    constexpr M(const Sreg& s, const R32& i, Scale sc, const Imm32& d);
    /** Creates a memory using seg:disp(index64,scale) form. */
    constexpr M(const Sreg& s, const R64& i, Scale sc, const Imm32& d);
    /** Creates a memory using (base32,index32,scale) form. */
    constexpr M(const R32& b, const R32& i, Scale sc);
    /** Creates a memory using (base64,index64,scale) form. */
    constexpr M(const R64& b, const R64& i, Scale sc);
    /** Creates a memory using seg:(base32,index32,scale) form. */
    constexpr M(const Sreg& s, const R32& b, const R32& i, Scale sc);
    /** Creates a memory using seg:(base64,index64,scale) form. */
    constexpr M(const Sreg& s, const R64& b, const R64& i, Scale sc);
    /** Creates a memory using disp(base32,index32,scale) form. */
    constexpr M(const R32& b, const R32& i, Scale sc, const Imm32& d);
    /** Creates a memory using disp(base64,index64,scale) form. */
    constexpr M(const R64& b, const R64& i, Scale sc, const Imm32& d);
    /** Creates a memory using seg:disp(base32,index32,scale) form. */
    constexpr M(const Sreg& s, const R32& b, const R32& i, Scale sc, 
        const Imm32& d);
    /** Creates a memory using seg:disp(base64,index64,scale) form. */
    constexpr M(const Sreg& s, const R64& b, const R64& i, Scale sc, 
        const Imm32& d);
};

/** A byte operand in memory, usually expressed as a variable or array name,
    but pointed to by the DS:(E)SI or ES:(E)DI registers.
    In 64-bit mode, it is pointed to by the RSI or RDI registers.
*/
class M8 : public M {
  public:
		using M::M;
};

/** A word operand in memory, usually expressed as a variable or array name,
    but pointed to by the DS:(E)SI or ES:(E)DI registers. This nomenclature is
    used only with the string instructions.
*/
class M16 : public M {
  public:
		using M::M;
};

/** A doubleword operand in memory, usually expressed as a variable or array
    name, but pointed to by the DS:(E)SI or ES:(E)DI registers. This
    nomenclature is used only with the string instructions.
*/
class M32 : public M {
  public:
		using M::M;
};

/** A memory quadword operand in memory. */
class M64 : public M {
  public:
		using M::M;
};

/** A memory double quadword operand in memory. */
class M128 : public M {
  public:
		using M::M;
};

/** A 32-byte operand in memory. This nomenclature is used only with AVX
    instructions.
*/
class M256 : public M {
  public:
		using M::M;
};

/** A word integer operand in memory. This symbol designates integers that are
    used as operands for x87 FPU integer instructions.
*/
class M16Int : public M {
  public:
		using M::M;
};

/** A doubleword integer operand in memory. This symbol designates integers
    that are used as operands for x87 FPU integer instructions.
*/
class M32Int : public M {
  public:
		using M::M;
};

/** A quadword integer operand in memory. This symbol designates integers
    that are used as operands for x87 FPU integer instructions.
*/
class M64Int : public M {
  public:
		using M::M;
};

/** A single-precision floating-point operand in memory. This symbol designates
    floating-point values that are used as operands for x87 FPU floating-point
    instructions.
*/
class M32Fp : public M {
  public:
		using M::M;
};

/** A double-precision floating-point operand in memory. This symbol designates
    floating-point values that are used as operands for x87 FPU floating-point
    instructions.
*/
class M64Fp : public M {
  public:
		using M::M;
};

/** A double extended-precision floating-point operand in memory. This symbol
    designates floating-point values that are used as operands for x87 FPU
    floating-point instructions.
*/
class M80Fp : public M {
  public:
		using M::M;
};

/** A double extended-precision binary-coded-decimaly operand in memory. */
class M80Bcd : public M {
  public:
		using M::M;
};

/** A 2 byte operand in memory. */
class M2Byte : public M {
  public:
		using M::M;
};

/** A 28 byte operand in memory. */
class M28Byte : public M {
  public:
		using M::M;
};

/** A 108 byte operand in memory. */
class M108Byte : public M {
  public:
		using M::M;
};

/** A 512 byte operand in memory. */
class M512Byte : public M {
  public:
		using M::M;
};

/** A memory operand containing a far pointer composed of two numbers. The
    number to the left of the colon corresponds to the pointer's segment
    selector. The number to the right corresponds to its offset.
*/
class FarPtr1616 : public M {
  public:
		using M::M;
};

/** A memory operand containing a far pointer composed of two numbers. The
    number to the left of the colon corresponds to the pointer's segment
    selector. The number to the right corresponds to its offset.
*/
class FarPtr1632 : public M {
  public:
		using M::M;
};

/** A memory operand containing a far pointer composed of two numbers. The
    number to the left of the colon corresponds to the pointer's segment
    selector. The number to the right corresponds to its offset.
*/
class FarPtr1664 : public M {
  public:
		using M::M;
};

} // namespace x64asm

namespace std {

/** STL hash specialization. */
template <>
struct hash<x64asm::M> {
  size_t operator()(const x64asm::M& m) const;
};

/** STL swap overload. */
void swap(x64asm::M& lhs, x64asm::M& rhs);

/** I/O overload. */
ostream& operator<<(ostream& os, const x64asm::M& m);

} // namespace std

namespace x64asm {

inline M::M(const M& rhs) {
  val_ = rhs.val_;
}

inline M::M(M&& rhs) {
  val_ = rhs.val_;
}

inline M& M::operator=(const M& rhs) {
  M(rhs).swap(*this);
  return *this;
}

inline M& M::operator=(M&& rhs) {
  M(std::move(rhs)).swap(*this);
  return *this;
}

inline constexpr bool M::contains_seg() {
  return (val_ & (uint64_t)Mask::SEG) !=
    ((uint64_t)Null::SEG << (uint64_t)Index::SEG);
}

inline constexpr bool M::contains_base() {
  return (val_ & (uint64_t)Mask::BASE) !=
    (r_null() << (uint64_t)Index::BASE);
}

inline constexpr bool M::contains_index() {
  return (val_ & (uint64_t)Mask::INDEX) !=
    (r_null() << (uint64_t)Index::INDEX);
}

inline constexpr bool M::addr_or()  {
  return val_ & (uint64_t)Mask::ADDR_OR;
}

inline constexpr bool M::rip_offset() {
  return val_ & (uint64_t)Mask::RIP;
}

inline constexpr Sreg M::get_seg() {
  return Sreg {(val_ & (uint64_t)Mask::SEG) >> (uint64_t)Index::SEG};
}

inline constexpr R64 M::get_base() {
  return R64 {(val_ & (uint64_t)Mask::BASE) >> (uint64_t)Index::BASE};
}

inline constexpr R64 M::get_index() {
  return R64 {(val_ & (uint64_t)Mask::INDEX) >> (uint64_t)Index::INDEX};
}

inline constexpr Scale M::get_scale() {
  return (Scale)((val_ & (uint64_t)Mask::SCALE) >> (uint64_t)Index::SCALE);
}

inline constexpr Imm32 M::get_disp() {
  return Imm32 {(uint32_t)(val_ & (uint64_t)Mask::DISP)};
}

inline void M::set_seg(const Sreg& seg) {
  val_ &= ~(uint64_t)Mask::SEG;
  val_ |= (uint64_t)seg << (uint64_t)Index::SEG;
}

inline void M::set_base(const R& base) {
  val_ &= ~(uint64_t)Mask::BASE;
  val_ |= (uint64_t)base << (uint64_t)Index::BASE;
}

inline void M::set_index(const R& index) {
  val_ &= ~(uint64_t)Mask::INDEX;
  val_ |= (uint64_t)index << (uint64_t)Index::INDEX;
}

inline void M::set_scale(Scale scale) {
  val_ &= ~(uint64_t)Mask::SCALE;
  val_ |= (uint64_t)scale << (uint64_t)Index::SCALE;
}

inline void M::set_disp(const Imm32& disp) {
  val_ &= ~(uint64_t)Mask::DISP;
  val_ |= (uint64_t)disp << (uint64_t)Index::DISP;
}

inline void M::set_addr_or(bool addr_or) {
  if (addr_or) {
    val_ |= (uint64_t)Mask::ADDR_OR;
  } else {
    val_ &= ~(uint64_t)Mask::ADDR_OR;
  }
}

inline void M::set_rip_offset(bool rip) {
  if (rip) {
    val_ |= (uint64_t)Mask::RIP;
  } else {
    val_ &= ~(uint64_t)Mask::RIP;
  }
}

inline void M::clear_seg() {
  set_seg(s_null());
}

inline void M::clear_base() {
  set_base(r_null());
}

inline void M::clear_index() {
  set_index(r_null());
}

inline constexpr bool M::operator<(const M& rhs) {
  return val_ < rhs.val_;
}

inline constexpr bool M::operator==(const M& rhs) {
  return val_ == rhs.val_;
}

inline constexpr bool M::operator!=(const M& rhs) {
  return val_ != rhs.val_;
}

inline constexpr size_t M::hash() {
  return val_;
}

inline void M::swap(M& rhs) {
  std::swap(val_, rhs.val_);
}

inline constexpr R64 M::r_null() {
  return R64((uint64_t)Null::REG);
}

inline constexpr Sreg M::s_null() {
  return Sreg((uint64_t)Null::SEG);
}

inline constexpr uint64_t M::init(const Imm32& d, const R& b, const R& i, 
    Scale sc, const Sreg& s, uint64_t addr_or, uint64_t rip) {
  return ((uint64_t)d & (uint64_t)Mask::DISP) |
    ((uint64_t)b << (uint64_t)Index::BASE) |
    ((uint64_t)i << (uint64_t)Index::INDEX) |
    ((uint64_t)sc << (uint64_t)Index::SCALE) |
    ((uint64_t)s << (uint64_t)Index::SEG) |
    (addr_or << (uint64_t)Index::ADDR_OR) |
    (rip << (uint64_t)Index::RIP);
}

inline constexpr M::M(const Imm32& d) :
    Operand(init(d, r_null(), r_null(), Scale::TIMES_1, s_null(), 0, 0)) {
}

inline constexpr M::M(const Sreg& s, const Imm32& d) :
    Operand(init(d, r_null(), r_null(), Scale::TIMES_1, s, 0, 0)) {
  }

inline constexpr M::M(const R32& b) :
    Operand(init(Imm32(0), b, r_null(), Scale::TIMES_1, s_null(), 1, 0)) {
}

inline constexpr M::M(const R64& b) :
    Operand(init(Imm32(0), b, r_null(), Scale::TIMES_1, s_null(), 0, 0)) {
}

inline constexpr M::M(Rip rip) :
    Operand(init(Imm32(0), r_null(), r_null(), Scale::TIMES_1, s_null(), 0, 1)) {
}

inline constexpr M::M(const Sreg& s, const R32& b) :
    Operand(init(Imm32(0), b, r_null(), Scale::TIMES_1, s, 1, 0)) {
}

inline constexpr M::M(const Sreg& s, const R64& b) :
    Operand(init(Imm32(0), b, r_null(), Scale::TIMES_1, s, 0, 0)) {
}

inline constexpr M::M(const Sreg& s, Rip rip) :
    Operand(init(Imm32(0), r_null(), r_null(), Scale::TIMES_1, s, 0, 1)) {
}

inline constexpr M::M(const R32& b, const Imm32& d) :
    Operand(init(d, b, r_null(), Scale::TIMES_1, s_null(), 1, 0)) {
}

inline constexpr M::M(const R64& b, const Imm32& d) :
    Operand(init(d, b, r_null(), Scale::TIMES_1, s_null(), 0, 0)) {
}

inline constexpr M::M(Rip rip, const Imm32& d) :
    Operand(init(d, r_null(), r_null(), Scale::TIMES_1, s_null(), 0, 1)) {
}

inline constexpr M::M(const Sreg& s, const R32& b, const Imm32& d) :
    Operand(init(d, b, r_null(), Scale::TIMES_1, s, 1, 0)) {
}

inline constexpr M::M(const Sreg& s, const R64& b, const Imm32& d) :
    Operand(init(d, b, r_null(), Scale::TIMES_1, s, 0, 0)) {
}

inline constexpr M::M(const Sreg& s, Rip rip, const Imm32& d) :
    Operand(init(d, r_null(), r_null(), Scale::TIMES_1, s, 0, 1)) {
}

inline constexpr M::M(const R32& i, Scale sc) :
    Operand(init(Imm32(0), r_null(), i, sc, s_null(), 1, 0)) {
}

inline constexpr M::M(const R64& i, Scale sc) :
    Operand(init(Imm32(0), r_null(), i, sc, s_null(), 0, 0)) {
}

inline constexpr M::M(const Sreg& s, const R32& i, Scale sc) :
    Operand(init(Imm32(0), r_null(), i, sc, s, 1, 0)) {
}

inline constexpr M::M(const Sreg& s, const R64& i, Scale sc) :
    Operand(init(Imm32(0), r_null(), i, sc, s, 0, 0)) {
}

inline constexpr M::M(const R32& i, Scale sc, const Imm32& d) :
    Operand(init(d, r_null(), i, sc, s_null(), 1, 0)) {
}

inline constexpr M::M(const R64& i, Scale sc, const Imm32& d) :
    Operand(init(d, r_null(), i, sc, s_null(), 0, 0)) {
}

inline constexpr M::M(const Sreg& s, const R32& i, Scale sc, const Imm32& d) :
    Operand(init(d, r_null(), i, sc, s, 1, 0)) {
}

inline constexpr M::M(const Sreg& s, const R64& i, Scale sc, const Imm32& d) :
    Operand(init(d, r_null(), i, sc, s, 0, 0)) {
}

inline constexpr M::M(const R32& b, const R32& i, Scale sc) :
    Operand(init(Imm32(0), b, i, sc, s_null(), 1, 0)) {
}

inline constexpr M::M(const R64& b, const R64& i, Scale sc) :
    Operand(init(Imm32(0), b, i, sc, s_null(), 0, 0)) {
}

inline constexpr M::M(const Sreg& s, const R32& b, const R32& i, Scale sc) :
    Operand(init(Imm32(0), b, i, sc, s, 1, 0)) {
}

inline constexpr M::M(const Sreg& s, const R64& b, const R64& i, Scale sc) :
    Operand(init(Imm32(0), b, i, sc, s, 0, 0)) {
}

inline constexpr M::M(const R32& b, const R32& i, Scale sc, const Imm32& d) :
    Operand(init(d, b, i, sc, s_null(), 1, 0)) {
}

inline constexpr M::M(const R64& b, const R64& i, Scale sc, const Imm32& d) :
    Operand(init(d, b, i, sc, s_null(), 0, 0)) {
}

inline constexpr M::M(const Sreg& s, const R32& b, const R32& i, Scale sc, 
    const Imm32& d) :
    Operand(init(d, b, i, sc, s, 1, 0)) {
}

inline constexpr M::M(const Sreg& s, const R64& b, const R64& i, Scale sc, 
    const Imm32& d) :
    Operand(init(d, b, i, sc, s, 0, 0)) {
}

} // namespace x64asm

namespace std {

inline size_t hash<x64asm::M>::operator()(const x64asm::M& m) const {
  return m.hash();
}

inline void swap(x64asm::M& lhs, x64asm::M& rhs) {
  lhs.swap(rhs);
}

inline ostream& operator<<(ostream& os, const x64asm::M& m) {
  return m.write_att(os);
}

} // namespace std

#endif
