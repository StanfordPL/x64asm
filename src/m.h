/*
Copyright 2013-2015 Stanford University

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

class Mem : public Operand {
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
  /** Returns true if this memory contains a segment register. */
  constexpr bool contains_seg() const {
    return (val_ & (uint64_t)Mask::SEG) != ((uint64_t)Null::SEG << (uint64_t)Index::SEG);
  }
  /** Returns true if this memory contains a base register. */
  constexpr bool contains_base() const {
    return (val_ & (uint64_t)Mask::BASE) != (r_null() << (uint64_t)Index::BASE);
  }
  /** Returns true if this memory contains an index register. */
  constexpr bool contains_index() const {
    return (val_ & (uint64_t)Mask::INDEX) != (r_null() << (uint64_t)Index::INDEX);
  }

  /** Returns true if this memory uses a 32-bit address override. */
  constexpr bool addr_or() const {
    return val_ & (uint64_t)Mask::ADDR_OR;
  }
  /** Returns true if this memory uses RIP+offset form. */
  constexpr bool rip_offset() const {
    return val_ & (uint64_t)Mask::RIP;
  }

  /** Returns this memory's segment register; undefined if absent. */
  constexpr Sreg get_seg() const {
    return {(val_ & (uint64_t)Mask::SEG) >> (uint64_t)Index::SEG};
  }
  /** Returns this memory's base register; undefined if absent. */
  constexpr R64 get_base() const {
    return {(val_ & (uint64_t)Mask::BASE) >> (uint64_t)Index::BASE};
  }
  /** Returns this memory's index register; undefined if absent. */
  constexpr R64 get_index() const {
    return {(val_ & (uint64_t)Mask::INDEX) >> (uint64_t)Index::INDEX};
  }
  /** Returns this memory's index scaling constant; 1 if absent. */
  constexpr Scale get_scale() const {
    return (Scale)((val_ & (uint64_t)Mask::SCALE) >> (uint64_t)Index::SCALE);
  }
  /** Returns this memory's displacement; 0 if absent. */
  constexpr Imm32 get_disp() const {
    return {(uint32_t)(val_ & (uint64_t)Mask::DISP)};
  }

  /** Sets this memory's segment register. */
  void set_seg(const Sreg& seg) {
    val_ &= ~(uint64_t)Mask::SEG;
    val_ |= (uint64_t)seg << (uint64_t)Index::SEG;
  }
  /** Sets this memory's base register. */
  void set_base(const R& base) {
    val_ &= ~(uint64_t)Mask::BASE;
    val_ |= (uint64_t)base << (uint64_t)Index::BASE;
  }
  /** Sets this memory's index register. */
  void set_index(const R& index) {
    val_ &= ~(uint64_t)Mask::INDEX;
    val_ |= (uint64_t)index << (uint64_t)Index::INDEX;
  }
  /** Sets this memory's scale register. */
  void set_scale(Scale scale) {
    val_ &= ~(uint64_t)Mask::SCALE;
    val_ |= (uint64_t)scale << (uint64_t)Index::SCALE;
  }
  /** Sets this memory's displacement. */
  void set_disp(const Imm32& disp) {
    val_ &= ~(uint64_t)Mask::DISP;
    val_ |= (uint64_t)disp << (uint64_t)Index::DISP;
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
    set_seg(s_null());
  }
  /** Removes the base register from this memory. */
  void clear_base() {
    set_base(r_null());
  }
  /** Remvoes the index register from this memory. */
  void clear_index() {
    set_index(r_null());
  }

  /** Returns true if this memory is well-formed: all present registers are
    well formed, the index register is not rsp, and that the RIP+offset() bit
    is set only if base and index registers are absent.
  */
  bool check() const;

  /** Comparison based on on val_. */
  constexpr bool operator<(const Mem& rhs) const {
    return val_ < rhs.val_;
  }
  /** Comparison based on on val_. */
  constexpr bool operator==(const Mem& rhs) const {
    return val_ == rhs.val_;
  }
  /** Comparison based on on val_. */
  constexpr bool operator!=(const Mem& rhs) const {
    return !(*this == rhs);
  }

  /** STL-compliant hash. */
  constexpr size_t hash() const {
    return val_;
  }
  /** STL-compliant swap. */
  void swap(Mem& rhs) {
    std::swap(val_, rhs.val_);
  }

  /** Reads a memory reference from an istream using at&t syntax. */
  std::istream& read_att(std::istream& is);
  /** Writes this memory reference to an ostream using at&t syntax. */
  std::ostream& write_att(std::ostream& os) const;

protected:
  /** Helper method: returns a null register. */
  static constexpr R64 r_null() {
    return {(uint64_t)Null::REG};
  }
  /** Helper method: returns a null segment register. */
  static constexpr Sreg s_null() {
    return {(uint64_t)Null::SEG};
  }
  /** Helper method: initializes all internal fields. */
  static constexpr uint64_t init(const Imm32& d, const R& b, const R& i,
                                 Scale sc, const Sreg& s, uint64_t addr_or, uint64_t rip) {
    return ((uint64_t)d & (uint64_t)Mask::DISP) |
           ((uint64_t)b << (uint64_t)Index::BASE) |
           ((uint64_t)i << (uint64_t)Index::INDEX) |
           ((uint64_t)sc << (uint64_t)Index::SCALE) |
           ((uint64_t)s << (uint64_t)Index::SEG) |
           (addr_or << (uint64_t)Index::ADDR_OR) |
           (rip << (uint64_t)Index::RIP);
  }


  /** Creates an operand with a type and no underlying value. */
  constexpr Mem(Type t) : Operand(t) {}
  /** Creates an operand with a type and one underlying value. */
  constexpr Mem(Type t, uint64_t val) : Operand(t, val) {}
  /** Creates an operand with a type and two underlying values. */
  constexpr Mem(Type t, uint64_t val, uint64_t val2) : Operand(t, val, val2) {}

};

/** An operand in memory. */
template <class T>
class M : public Mem {
public:
  /** Creates a memory using an existing one.  Used to change types. */
  template <typename U>
  constexpr M(const M<U>& m) :
    Mem(T::m_type(), init(m.get_disp(), m.get_base(), m.get_index(), m.get_scale(),
                              m.get_seg(), m.addr_or(), m.rip_offset())) {
  }
  /** Creates a memory using an existing one.  Used to change types. */
  template <typename U>
  constexpr M(const M<U>& m, Type t) :
    Mem(t, init(m.get_disp(), m.get_base(), m.get_index(), m.get_scale(),
                    m.get_seg(), m.addr_or(), m.rip_offset())) {
  }
  /** Creates a memory using disp form. */
  constexpr M(const Imm32& d) :
    Mem(T::m_type(), init(d, r_null(), r_null(), Scale::TIMES_1, s_null(), 0, 0)) {
  }
  /** Creates a memory using seg:disp form. */
  constexpr M(const Sreg& s, const Imm32& d) :
    Mem(T::m_type(), init(d, r_null(), r_null(), Scale::TIMES_1, s, 0, 0)) {
  }
  /** Creates a memroy using (base64) form. */
  constexpr M(const R32& b) :
    Mem(T::m_type(), init(Imm32(0), b, r_null(), Scale::TIMES_1, s_null(), 1, 0)) {
  }
  /** Creates a memory using (base32) form. */
  constexpr M(const R64& b) :
    Mem(T::m_type(), init(Imm32(0), b, r_null(), Scale::TIMES_1, s_null(), 0, 0)) {
  }
  /** Creates a memory using RIP form. */
  constexpr M(Rip rip) :
    Mem(T::m_type(), init(Imm32(0), r_null(), r_null(), Scale::TIMES_1, s_null(), 0, 1)) {
  }
  /** Creates a memory using seg:base32 form. */
  constexpr M(const Sreg& s, const R32& b) :
    Mem(T::m_type(), init(Imm32(0), b, r_null(), Scale::TIMES_1, s, 1, 0)) {
  }
  /** Creates a memory using seg:base64 form. */
  constexpr M(const Sreg& s, const R64& b) :
    Mem(T::m_type(), init(Imm32(0), b, r_null(), Scale::TIMES_1, s, 0, 0)) {
  }
  /** Creates a memory using seg:RIP form. */
  constexpr M(const Sreg& s, Rip rip) :
    Mem(T::m_type(), init(Imm32(0), r_null(), r_null(), Scale::TIMES_1, s, 0, 1)) {
  }
  /** Creates a memory using disp(base32) form. */
  constexpr M(const R32& b, const Imm32& d) :
    Mem(T::m_type(), init(d, b, r_null(), Scale::TIMES_1, s_null(), 1, 0)) {
  }
  /** Creates a memory using disp(base64) form. */
  constexpr M(const R64& b, const Imm32& d) :
    Mem(T::m_type(), init(d, b, r_null(), Scale::TIMES_1, s_null(), 0, 0)) {
  }
  /** Creates a memory using RIP+disp form. */
  constexpr M(Rip rip, const Imm32& d) :
    Mem(T::m_type(), init(d, r_null(), r_null(), Scale::TIMES_1, s_null(), 0, 1)) {
  }
  /** Creates a memory using seg:disp(base32) form. */
  constexpr M(const Sreg& s, const R32& b, const Imm32& d) :
    Mem(T::m_type(), init(d, b, r_null(), Scale::TIMES_1, s, 1, 0)) {
  }
  /** Creates a memory using seg:disp(base64) form. */
  constexpr M(const Sreg& s, const R64& b, const Imm32& d) :
    Mem(T::m_type(), init(d, b, r_null(), Scale::TIMES_1, s, 0, 0)) {
  }
  /** Creates a memory using seg:RIP+disp form. */
  constexpr M(const Sreg& s, Rip rip, const Imm32& d) :
    Mem(T::m_type(), init(d, r_null(), r_null(), Scale::TIMES_1, s, 0, 1)) {
  }
  /** Creates a memory using (index32,scale) form. */
  constexpr M(const R32& i, Scale sc) :
    Mem(T::m_type(), init(Imm32(0), r_null(), i, sc, s_null(), 1, 0)) {
  }
  /** Creates a memory using (index64,scale) form. */
  constexpr M(const R64& i, Scale sc) :
    Mem(T::m_type(), init(Imm32(0), r_null(), i, sc, s_null(), 0, 0)) {
  }
  /** Creates a memory using seg:(index32,scale) form. */
  constexpr M(const Sreg& s, const R32& i, Scale sc) :
    Mem(T::m_type(), init(Imm32(0), r_null(), i, sc, s, 1, 0)) {
  }
  /** Creates a memory using seg:(index64,scale) form. */
  constexpr M(const Sreg& s, const R64& i, Scale sc) :
    Mem(T::m_type(), init(Imm32(0), r_null(), i, sc, s, 0, 0)) {
  }
  /** Creates a memory using disp(index32,scale) form. */
  constexpr M(const R32& i, Scale sc, const Imm32& d) :
    Mem(T::m_type(), init(d, r_null(), i, sc, s_null(), 1, 0)) {
  }
  /** Creates a memory using disp(index64,scale) form. */
  constexpr M(const R64& i, Scale sc, const Imm32& d) :
    Mem(T::m_type(), init(d, r_null(), i, sc, s_null(), 0, 0)) {
  }
  /** Creates a memory using seg:disp(index32,scale) form. */
  constexpr M(const Sreg& s, const R32& i, Scale sc, const Imm32& d) :
    Mem(T::m_type(), init(d, r_null(), i, sc, s, 1, 0)) {
  }
  /** Creates a memory using seg:disp(index64,scale) form. */
  constexpr M(const Sreg& s, const R64& i, Scale sc, const Imm32& d) :
    Mem(T::m_type(), init(d, r_null(), i, sc, s, 0, 0)) {
  }
  /** Creates a memory using (base32,index32,scale) form. */
  constexpr M(const R32& b, const R32& i, Scale sc) :
    Mem(T::m_type(), init(Imm32(0), b, i, sc, s_null(), 1, 0)) {
  }
  /** Creates a memory using (base64,index64,scale) form. */
  constexpr M(const R64& b, const R64& i, Scale sc) :
    Mem(T::m_type(), init(Imm32(0), b, i, sc, s_null(), 0, 0)) {
  }
  /** Creates a memory using seg:(base32,index32,scale) form. */
  constexpr M(const Sreg& s, const R32& b, const R32& i, Scale sc) :
    Mem(T::m_type(), init(Imm32(0), b, i, sc, s, 1, 0)) {
  }
  /** Creates a memory using seg:(base64,index64,scale) form. */
  constexpr M(const Sreg& s, const R64& b, const R64& i, Scale sc) :
    Mem(T::m_type(), init(Imm32(0), b, i, sc, s, 0, 0)) {
  }
  /** Creates a memory using disp(base32,index32,scale) form. */
  constexpr M(const R32& b, const R32& i, Scale sc, const Imm32& d) :
    Mem(T::m_type(), init(d, b, i, sc, s_null(), 1, 0)) {
  }
  /** Creates a memory using disp(base64,index64,scale) form. */
  constexpr M(const R64& b, const R64& i, Scale sc, const Imm32& d) :
    Mem(T::m_type(), init(d, b, i, sc, s_null(), 0, 0)) {
  }
  /** Creates a memory using seg:disp(base32,index32,scale) form. */
  constexpr M(const Sreg& s, const R32& b, const R32& i, Scale sc, const Imm32& d) :
    Mem(T::m_type(), init(d, b, i, sc, s, 1, 0)) {
  }
  /** Creates a memory using seg:disp(base64,index64,scale) form. */
  constexpr M(const Sreg& s, const R64& b, const R64& i, Scale sc, const Imm32& d) :
    Mem(T::m_type(), init(d, b, i, sc, s, 0, 0)) {
  }
};

/** A byte operand in memory, usually expressed as a variable or array name,
    but pointed to by the DS:(E)SI or ES:(E)DI registers.
    In 64-bit mode, it is pointed to by the RSI or RDI registers.
*/
class M8 : public M<M8> {
public:
  using M::M;

  /** Returns the type of this operand */
  static constexpr Type m_type() {
    return Type::M_8;
  }

};

/** A word operand in memory, usually expressed as a variable or array name,
    but pointed to by the DS:(E)SI or ES:(E)DI registers. This nomenclature is
    used only with the string instructions.
*/
class M16 : public M<M16> {
public:
  using M::M;

  /** Returns the type of this operand */
  static constexpr Type m_type() {
    return Type::M_16;
  }

};

/** A doubleword operand in memory, usually expressed as a variable or array
    name, but pointed to by the DS:(E)SI or ES:(E)DI registers. This
    nomenclature is used only with the string instructions.
*/
class M32 : public M<M32> {
public:
  using M::M;

  /** Returns the type of this operand */
  static constexpr Type m_type() {
    return Type::M_32;
  }

};

/** A memory quadword operand in memory. */
class M64 : public M<M64> {
public:
  using M::M;

  /** Returns the type of this operand */
  static constexpr Type m_type() {
    return Type::M_64;
  }

};

/** A memory double quadword operand in memory. */
class M128 : public M<M128>  {
public:
  using M::M;

  /** Returns the type of this operand */
  static constexpr Type m_type() {
    return Type::M_128;
  }
};

/** A 32-byte operand in memory. This nomenclature is used only with AVX
    instructions.
*/
class M256 : public M<M256>  {
public:
  using M::M;

  /** Returns the type of this operand */
  static constexpr Type m_type() {
    return Type::M_256;
  }
};

/** A word integer operand in memory. This symbol designates integers that are
    used as operands for x87 FPU integer instructions.
*/
class M16Int : public M<M16Int>  {
public:
  using M::M;


  /** Returns the type of this operand */
  static constexpr Type m_type() {
    return Type::M_16_INT;
  }

};

/** A doubleword integer operand in memory. This symbol designates integers
    that are used as operands for x87 FPU integer instructions.
*/
class M32Int : public M<M32Int>  {
public:
  using M::M;

  /** Returns the type of this operand */
  static constexpr Type m_type() {
    return Type::M_32_INT;
  }


};

/** A quadword integer operand in memory. This symbol designates integers
    that are used as operands for x87 FPU integer instructions.
*/
class M64Int : public M<M64Int>  {
public:
  using M::M;

  /** Returns the type of this operand */
  static constexpr Type m_type() {
    return Type::M_64_INT;
  }


};

/** A single-precision floating-point operand in memory. This symbol designates
    floating-point values that are used as operands for x87 FPU floating-point
    instructions.
*/
class M32Fp : public M<M32Fp>  {
public:
  using M::M;

  /** Returns the type of this operand */
  static constexpr Type m_type() {
    return Type::M_32_FP;
  }


};

/** A double-precision floating-point operand in memory. This symbol designates
    floating-point values that are used as operands for x87 FPU floating-point
    instructions.
*/
class M64Fp : public M<M64Fp>  {
public:
  using M::M;

  /** Returns the type of this operand */
  static constexpr Type m_type() {
    return Type::M_64_FP;
  }
};

/** A double extended-precision floating-point operand in memory. This symbol
    designates floating-point values that are used as operands for x87 FPU
    floating-point instructions.
*/
class M80Fp : public M<M80Fp>  {
public:
  using M::M;

  /** Returns the type of this operand */
  static constexpr Type m_type() {
    return Type::M_80_FP;
  }
};

/** A double extended-precision binary-coded-decimaly operand in memory. */
class M80Bcd : public M<M80Bcd>  {
public:
  using M::M;

  /** Returns the type of this operand */
  static constexpr Type m_type() {
    return Type::M_80_BCD;
  }

};

/** A 2 byte operand in memory. */
class M2Byte : public M<M2Byte>  {
public:
  using M::M;

  /** Returns the type of this operand */
  static constexpr Type m_type() {
    return Type::M_2_BYTE;
  }
};

/** A 28 byte operand in memory. */
class M28Byte : public M<M28Byte>  {
public:
  using M::M;

  /** Returns the type of this operand */
  static constexpr Type m_type() {
    return Type::M_28_BYTE;
  }

};

/** A 108 byte operand in memory. */
class M108Byte : public M<M108Byte>  {
public:
  using M::M;

  /** Returns the type of this operand */
  static constexpr Type m_type() {
    return Type::M_108_BYTE;
  }

};

/** A 512 byte operand in memory. */
class M512Byte : public M<M512Byte>  {
public:
  using M::M;

  /** Returns the type of this operand */
  static constexpr Type m_type() {
    return Type::M_512_BYTE;
  }

};

/** A memory operand containing a far pointer composed of two numbers. The
    number to the left of the colon corresponds to the pointer's segment
    selector. The number to the right corresponds to its offset.
*/
class FarPtr1616 : public M<FarPtr1616>  {
public:
  using M::M;

  /** Returns the type of this operand */
  static constexpr Type m_type() {
    return Type::FAR_PTR_16_16;
  }


};

/** A memory operand containing a far pointer composed of two numbers. The
    number to the left of the colon corresponds to the pointer's segment
    selector. The number to the right corresponds to its offset.
*/
class FarPtr1632 : public M<FarPtr1632>  {
public:
  using M::M;

  /** Returns the type of this operand */
  static constexpr Type m_type() {
    return Type::FAR_PTR_16_32;
  }

};

/** A memory operand containing a far pointer composed of two numbers. The
    number to the left of the colon corresponds to the pointer's segment
    selector. The number to the right corresponds to its offset.
*/
class FarPtr1664 : public M<FarPtr1664>  {
public:
  using M::M;

  /** Returns the type of this operand */
  static constexpr Type m_type() {
    return Type::FAR_PTR_16_64;
  }

};

} // namespace x64asm


namespace std {

/** STL hash specialization. */
template <class T>
struct hash<x64asm::M<T>> {
  size_t operator()(const x64asm::M<T>& m) const {
    return m.hash();
  }
};

template <>
struct hash<x64asm::Mem> {
  size_t operator()(const x64asm::Mem& m) const {
    return m.hash();
  }
};



/** iostream overload. */
inline istream& operator>>(istream& is, x64asm::Mem& m) {
  return m.read_att(is);
}
/** iostream overload. */
inline ostream& operator<<(ostream& os, const x64asm::Mem& m) {
  return m.write_att(os);
}

} // namespace std

#endif
