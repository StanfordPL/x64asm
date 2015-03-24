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
template <class T>
class M : public Operand {
private:
  /** Value constants */
  static constexpr uint64_t reg_null_ = 0x10ull;
  static constexpr uint64_t seg_null_ = 0x07ull;
  /** Mask constants */
  static constexpr uint64_t disp_mask_    = 0xffffffffull;
  static constexpr uint64_t base_mask_    = 0x1full;
  static constexpr uint64_t index_mask_   = 0x1full;
  static constexpr uint64_t scale_mask_   = 0x3ull;
  static constexpr uint64_t seg_mask_     = 0x7ull;
  static constexpr uint64_t addr_or_mask_ = 0x1ull;
  static constexpr uint64_t rip_mask_     = 0x1ull;
  /** Index constants */
  static constexpr size_t disp_idx_    = 0;
  static constexpr size_t base_idx_    = 32;
  static constexpr size_t index_idx_   = 40;
  static constexpr size_t scale_idx_   = 48;
  static constexpr size_t seg_idx_     = 56;
  static constexpr size_t addr_or_idx_ = 60;
  static constexpr size_t rip_idx_     = 61;

public:
  /** Creates a memory using an existing one.  Used to change types. */
  template <typename U>
  constexpr M(const M<U>& m) : 
    Operand(T::m_type(), m.val(), m.val2()) {}
  /** Creates a memory using an existing one.  Used to change types. */
  template <typename U>
  constexpr M(const M<U>& m, Type t) : 
    Operand(t, m.val(), m.val2()) {}
  /** Creates a memory using disp form. */
  constexpr M(const Imm32& d) :
    Operand(T::m_type(), init(d, reg_null_, reg_null_, Scale::TIMES_1, seg_null_, 0, 0)) {}
  /** Creates a memory using seg:disp form. */
  constexpr M(const Sreg& s, const Imm32& d) :
    Operand(T::m_type(), init(d, reg_null_, reg_null_, Scale::TIMES_1, s, 0, 0)) {}
  /** Creates a memroy using (base64) form. */
  constexpr M(const R32& b) :
    Operand(T::m_type(), init(Imm32(0), b, reg_null_, Scale::TIMES_1, seg_null_, 1, 0)) {}
  /** Creates a memory using (base32) form. */
  constexpr M(const R64& b) :
    Operand(T::m_type(), init(Imm32(0), b, reg_null_, Scale::TIMES_1, seg_null_, 0, 0)) {}
  /** Creates a memory using RIP form. */
  constexpr M(Rip rip) :
    Operand(T::m_type(), init(Imm32(0), reg_null_, reg_null_, Scale::TIMES_1, seg_null_, 0, 1)) {}
  /** Creates a memory using seg:base32 form. */
  constexpr M(const Sreg& s, const R32& b) :
    Operand(T::m_type(), init(Imm32(0), b, reg_null_, Scale::TIMES_1, s, 1, 0)) {}
  /** Creates a memory using seg:base64 form. */
  constexpr M(const Sreg& s, const R64& b) :
    Operand(T::m_type(), init(Imm32(0), b, reg_null_, Scale::TIMES_1, s, 0, 0)) {}
  /** Creates a memory using seg:RIP form. */
  constexpr M(const Sreg& s, Rip rip) :
    Operand(T::m_type(), init(Imm32(0), reg_null_, reg_null_, Scale::TIMES_1, s, 0, 1)) {}
  /** Creates a memory using disp(base32) form. */
  constexpr M(const R32& b, const Imm32& d) :
    Operand(T::m_type(), init(d, b, reg_null_, Scale::TIMES_1, seg_null_, 1, 0)) {}
  /** Creates a memory using disp(base64) form. */
  constexpr M(const R64& b, const Imm32& d) :
    Operand(T::m_type(), init(d, b, reg_null_, Scale::TIMES_1, seg_null_, 0, 0)) {}
  /** Creates a memory using RIP+disp form. */
  constexpr M(Rip rip, const Imm32& d) :
    Operand(T::m_type(), init(d, reg_null_, reg_null_, Scale::TIMES_1, seg_null_, 0, 1)) {}
  /** Creates a memory using seg:disp(base32) form. */
  constexpr M(const Sreg& s, const R32& b, const Imm32& d) :
    Operand(T::m_type(), init(d, b, reg_null_, Scale::TIMES_1, s, 1, 0)) {}
  /** Creates a memory using seg:disp(base64) form. */
  constexpr M(const Sreg& s, const R64& b, const Imm32& d) :
    Operand(T::m_type(), init(d, b, reg_null_, Scale::TIMES_1, s, 0, 0)) {}
  /** Creates a memory using seg:RIP+disp form. */
  constexpr M(const Sreg& s, Rip rip, const Imm32& d) :
    Operand(T::m_type(), init(d, reg_null_, reg_null_, Scale::TIMES_1, s, 0, 1)) {}
  /** Creates a memory using (index32,scale) form. */
  constexpr M(const R32& i, Scale sc) :
    Operand(T::m_type(), init(Imm32(0), reg_null_, i, sc, seg_null_, 1, 0)) {}
  /** Creates a memory using (index64,scale) form. */
  constexpr M(const R64& i, Scale sc) :
    Operand(T::m_type(), init(Imm32(0), reg_null_, i, sc, seg_null_, 0, 0)) {}
  /** Creates a memory using seg:(index32,scale) form. */
  constexpr M(const Sreg& s, const R32& i, Scale sc) :
    Operand(T::m_type(), init(Imm32(0), reg_null_, i, sc, s, 1, 0)) {}
  /** Creates a memory using seg:(index64,scale) form. */
  constexpr M(const Sreg& s, const R64& i, Scale sc) :
    Operand(T::m_type(), init(Imm32(0), reg_null_, i, sc, s, 0, 0)) {}
  /** Creates a memory using disp(index32,scale) form. */
  constexpr M(const R32& i, Scale sc, const Imm32& d) :
    Operand(T::m_type(), init(d, reg_null_, i, sc, seg_null_, 1, 0)) {}
  /** Creates a memory using disp(index64,scale) form. */
  constexpr M(const R64& i, Scale sc, const Imm32& d) :
    Operand(T::m_type(), init(d, reg_null_, i, sc, seg_null_, 0, 0)) {}
  /** Creates a memory using seg:disp(index32,scale) form. */
  constexpr M(const Sreg& s, const R32& i, Scale sc, const Imm32& d) :
    Operand(T::m_type(), init(d, reg_null_, i, sc, s, 1, 0)) {}
  /** Creates a memory using seg:disp(index64,scale) form. */
  constexpr M(const Sreg& s, const R64& i, Scale sc, const Imm32& d) :
    Operand(T::m_type(), init(d, reg_null_, i, sc, s, 0, 0)) {}
  /** Creates a memory using (base32,index32,scale) form. */
  constexpr M(const R32& b, const R32& i, Scale sc) :
    Operand(T::m_type(), init(Imm32(0), b, i, sc, seg_null_, 1, 0)) {}
  /** Creates a memory using (base64,index64,scale) form. */
  constexpr M(const R64& b, const R64& i, Scale sc) :
    Operand(T::m_type(), init(Imm32(0), b, i, sc, seg_null_, 0, 0)) {}
  /** Creates a memory using seg:(base32,index32,scale) form. */
  constexpr M(const Sreg& s, const R32& b, const R32& i, Scale sc) :
    Operand(T::m_type(), init(Imm32(0), b, i, sc, s, 1, 0)) {}
  /** Creates a memory using seg:(base64,index64,scale) form. */
  constexpr M(const Sreg& s, const R64& b, const R64& i, Scale sc) :
    Operand(T::m_type(), init(Imm32(0), b, i, sc, s, 0, 0)) {}
  /** Creates a memory using disp(base32,index32,scale) form. */
  constexpr M(const R32& b, const R32& i, Scale sc, const Imm32& d) :
    Operand(T::m_type(), init(d, b, i, sc, seg_null_, 1, 0)) {}
  /** Creates a memory using disp(base64,index64,scale) form. */
  constexpr M(const R64& b, const R64& i, Scale sc, const Imm32& d) :
    Operand(T::m_type(), init(d, b, i, sc, seg_null_, 0, 0)) {}
  /** Creates a memory using seg:disp(base32,index32,scale) form. */
  constexpr M(const Sreg& s, const R32& b, const R32& i, Scale sc, const Imm32& d) :
    Operand(T::m_type(), init(d, b, i, sc, s, 1, 0)) {}
  /** Creates a memory using seg:disp(base64,index64,scale) form. */
  constexpr M(const Sreg& s, const R64& b, const R64& i, Scale sc, const Imm32& d) :
    Operand(T::m_type(), init(d, b, i, sc, s, 0, 0)) {}

  /** Returns true if this memory contains a segment register. */
  constexpr bool contains_seg() {
    return val(seg_mask_, seg_idx_) != seg_null_;
  }
  /** Returns true if this memory contains a base register. */
  constexpr bool contains_base() {
    return val(base_mask_, base_idx_) != reg_null_;
  }
  /** Returns true if this memory contains an index register. */
  constexpr bool contains_index() {
    return val(index_mask_, index_idx_) != reg_null_;
  }

  /** Returns true if this memory uses a 32-bit address override. */
  constexpr bool addr_or() {
    return val(addr_or_mask_, addr_or_idx_);
  }
  /** Returns true if this memory uses RIP+offset form. */
  constexpr bool rip_offset() {
    return val(rip_mask_, rip_idx_);
  }

  /** Returns this memory's segment register; undefined if absent. */
  constexpr Sreg get_seg() {
    return {val(seg_mask_, seg_idx_)};
  }
  /** Returns this memory's base register; undefined if absent. */
  constexpr R64 get_base() {
    return {val(base_mask_, base_idx_)};
  }
  /** Returns this memory's index register; undefined if absent. */
  constexpr R64 get_index() {
    return {val(index_mask_, index_idx_)};
  }
  /** Returns this memory's index scaling constant; 1 if absent. */
  constexpr Scale get_scale() {
    return (Scale)val(scale_mask_, scale_idx_);
  }
  /** Returns this memory's displacement; 0 if absent. */
  constexpr Imm32 get_disp() {
    return {(uint32_t)val(disp_mask_, disp_idx_)};
  }

  /** Sets this memory's segment register. */
  void set_seg(const Sreg& seg) {
    set_val(seg, seg_mask_, seg_idx_);
  }
  /** Sets this memory's base register. */
  void set_base(const R& base) {
    set_val(base, base_mask_, base_idx_);
  }
  /** Sets this memory's index register. */
  void set_index(const R& index) {
    set_val(index, index_mask_, index_idx_);
  }
  /** Sets this memory's scale register. */
  void set_scale(Scale scale) {
    set_val((uint64_t)scale, scale_mask_, scale_idx_);
  }
  /** Sets this memory's displacement. */
  void set_disp(const Imm32& disp) {
    set_val(disp, disp_mask_, disp_idx_);
  }
  /** Sets the 32-bit address override bit for this memory. */
  void set_addr_or(bool addr_or) {
    set_val(addr_or ? 1 : 0, addr_or_mask_, addr_or_idx_);
  }
  /** Sets the RIP+offset form flag for this memory. */
  void set_rip_offset(bool rip) {
    set_val(rip ? 1 : 0, rip_mask_, rip_idx_);
  }

  /** Removes the segment register from this memory. */
  void clear_seg() {
    set_val(seg_null_, seg_idx_, seg_null_);
  }
  /** Removes the base register from this memory. */
  void clear_base() {
    set_val(reg_null_, base_idx_, reg_null_);
  }
  /** Remvoes the index register from this memory. */
  void clear_index() {
    set_val(reg_null_, index_mask_, index_idx_);
  }

  /** Returns true if this memory is well-formed: all present registers are
    well formed, the index register is not rsp, and that the RIP+offset() bit
    is set only if base and index registers are absent.
  */
  bool check() const;

  /** @todo This method is undefined. */
  std::istream& read_att(std::istream& is) {
    is.setstate(std::ios::failbit);
    return is;
  }
  /** Writes this xmm register to an ostream using at&t syntax. */
  std::ostream& write_att(std::ostream& os) const;

private:
  /** Helper method: initializes all internal fields. */
  static constexpr uint64_t init(uint64_t d, uint64_t b, uint64_t i,
      Scale sc, uint64_t s, uint64_t ao, uint64_t r) {
    return (d  << disp_idx_) |
           (b  << base_idx_) |
           (i  << index_idx_) |
           ((uint64_t)sc << scale_idx_) |
           (s  << seg_idx_) |
           (ao << addr_or_idx_) |
           (r  << rip_idx_);
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

/** Implementation of functions; needed due to templating */
#include "src/m_cc.h"

namespace std {

/** iostream overload. */
template <class T>
inline istream& operator>>(istream& is, x64asm::M<T>& m) {
  return m.read_att(is);
}
/** iostream overload. */
template <class T>
inline ostream& operator<<(ostream& os, const x64asm::M<T>& m) {
  return m.write_att(os);
}

} // namespace std

#endif
