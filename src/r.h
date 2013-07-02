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

#ifndef X64ASM_SRC_R_H
#define X64ASM_SRC_R_H

#include <cassert>
#include <iostream>

#include "src/operand.h"

namespace x64asm {

/** A general-purpose register. */
class R : public Operand {
  public:
    /** Copy constructor. */
    R(const R& rhs);
    /** Move constructor. */
    R(R&& rhs);
    /** Copy assignment operator. */
    R& operator=(const R& rhs);
    /** Move assignment operator. */
    R& operator=(R&& rhs);

    /** Conversion based on underlying value. */
    constexpr operator uint64_t();

    /** STL-compliant hash. */
    constexpr size_t hash();
    /** STL-compliant swap. */
    void swap(R& rhs);

  protected:
    /** Direct access to this constructor is disallowed. */
    constexpr R(uint64_t val);
};

/** One of the byte general-purpose registers: AL, CL, DL, BL. */
class Rl : public R {
    // Needs access to constructor.
    friend class Constants;

  public:
    /** Returns true if this register is well-formed. */
    constexpr bool check();

    /** Comparison based on on val_. */
    constexpr bool operator<(const Rl& rhs);
    /** Comparison based on on val_. */
    constexpr bool operator==(const Rl& rhs);
    /** Comparison based on on val_. */
    constexpr bool operator!=(const Rl& rhs);

    /** Writes this register to an ostream using at&t syntax. */
    std::ostream& write_att(std::ostream& os) const;

  protected:
    /** Direct access to this constructor is disallowed. */
    constexpr Rl(uint64_t val);
};

/** The byte general-purpose register AL. */
class Al : public Rl {
    // Needs access to constructor.
    friend class Constants;

  public:
    /** Returns true if this register is well-formed. */
    constexpr bool check();

  private:
    /** Direct access to this constructor is disallowed. */
    constexpr Al();
};

/** The byte general-purpose register CL. */
class Cl : public Rl {
    // Needs access to constructor.
    friend class Constants;

  public:
    /** Returns true if this register is well-formed. */
    constexpr bool check();

  private:
    /** Direct access to this constructor is disallowed. */
    constexpr Cl();
};

/** One of the byte general-purpose registers: AH, CH, DH, BH. */
class Rh : public R {
    // Needs access to constructor.
    friend class Constants;

  public:
    /** Returns true if this register is well-formed. */
    constexpr bool check();

    /** Comparison based on on val_. */
    constexpr bool operator<(const Rh& rhs);
    /** Comparison based on on val_. */
    constexpr bool operator==(const Rh& rhs);
    /** Comparison based on on val_. */
    constexpr bool operator!=(const Rh& rhs);

    /** Writes this register to an ostream using at&t syntax. */
    std::ostream& write_att(std::ostream& os) const;

  protected:
    /** Direct access to this constructor is disallowed. */
    constexpr Rh(uint64_t val);
};

/** One of the byte general-purpose registers: BPL, SPL, DIL and SIL; or one of
    the byte registers (R8B - R15B) available when using REX.R and 64-bit mode.
*/
class Rb : public R {
    // Needs access to constructor.
    friend class Constants;

  public:
    /** Returns true if this register is well-formed. */
    constexpr bool check();

    /** Comparison based on on val_. */
    constexpr bool operator<(const Rb& rhs);
    /** Comparison based on on val_. */
    constexpr bool operator==(const Rb& rhs);
    /** Comparison based on on val_. */
    constexpr bool operator!=(const Rb& rhs);

    /** Writes this register to an ostream using at&t syntax. */
    std::ostream& write_att(std::ostream& os) const;

  protected:
    /** Direct access to this constructor is disallowed. */
    constexpr Rb(uint64_t val);
};

/** One of the word general-purpose registers: AX, CX, DX, BX, SP, BP, SI, DI;
    or one of the word registers (R8W - R15W) available when using REX.R and
    64-bit mode.
*/
class R16 : public R {
    // Needs access to constructor.
    friend class Constants;

  public:
    /** Returns true if this register is well-formed. */
    constexpr bool check();

    /** Comparison based on on val_. */
    constexpr bool operator<(const R16& rhs);
    /** Comparison based on on val_. */
    constexpr bool operator==(const R16& rhs);
    /** Comparison based on on val_. */
    constexpr bool operator!=(const R16& rhs);

    /** Writes this register to an ostream using at&t syntax. */
    std::ostream& write_att(std::ostream& os) const;

  protected:
    /** Direct access to this constructor is disallowed. */
    constexpr R16(uint64_t val);
};

/** The word general-purpose register AX. */
class Ax : public R16 {
    // Needs access to constructor.
    friend class Constants;

  public:
    /** Returns true if this register is well-formed. */
    constexpr bool check();

  private:
    /** Direct access to this constructor is disallowed. */
    constexpr Ax();
};

/** The word general-purpose register DX. */
class Dx : public R16 {
    // Needs access to constructor.
    friend class Constants;

  public:
    /** Returns true if this register is well-formed. */
    constexpr bool check();

  private:
    /** Direct access to this constructor is disallowed. */
    constexpr Dx();
};

/** One of the doubleword general-purpose registers: EAX, ECX, EDX, EBX, ESP,
    EBP, ESI, EDI; or one of the doubleword registers (R8D - R15D) available
    when using REX.R in 64-bit mode.
*/
class R32 : public R {
    // Needs access to constructor.
    friend class Constants;
    // Needs access to constructor.
    friend class M;

  public:
    /** Returns true if this register is well-formed. */
    constexpr bool check();

    /** Comparison based on on val_. */
    constexpr bool operator<(const R32& rhs);
    /** Comparison based on on val_. */
    constexpr bool operator==(const R32& rhs);
    /** Comparison based on on val_. */
    constexpr bool operator!=(const R32& rhs);

    /** Writes this register to an ostream using at&t syntax. */
    std::ostream& write_att(std::ostream& os) const;

  protected:
    /** Direct access to this constructor is disallowed. */
    constexpr R32(uint64_t val);
};

/** The doubleword general-purpose register EAX. */
class Eax : public R32 {
    // Needs access to constructor.
    friend class Constants;

  public:
    /** Returns true if this register is well-formed. */
    constexpr bool check();

  private:
    /** Direct access to this constructor is disallowed. */
    constexpr Eax();
};

/** One of the quadword general-purpose registers: RAX, RBX, RCX, RDX, RDI, RSI,
    RBP, RSP, R8–R15. These are available when using REX.R and 64-bit mode.
*/
class R64 : public R {
    // Needs access to constructor.
    friend class Constants;
    // Needs access to constructor.
    friend class M;

  public:
    /** Returns true if this register is well-formed. */
    constexpr bool check();

    /** Comparison based on on val_. */
    constexpr bool operator<(const R64& rhs);
    /** Comparison based on on val_. */
    constexpr bool operator==(const R64& rhs);
    /** Comparison based on on val_. */
    constexpr bool operator!=(const R64& rhs);

    /** Writes this register to an ostream using at&t syntax. */
    std::ostream& write_att(std::ostream& os) const;

  protected:
    /** Direct access to this constructor is disallowed. */
    constexpr R64(uint64_t val);
};

/** The quadword general-purpose register RAX. */
class Rax : public R64 {
    // Needs access to constructor.
    friend class Constants;

  public:
    /** Returns true if this register is well-formed. */
    constexpr bool check();

  private:
    /** Direct access to this constructor is disallowed. */
    constexpr Rax();
};

} // namespace x64asm

namespace std {

/** STL hash specialization. */
template <>
struct hash<x64asm::R> {
  size_t operator()(const x64asm::R& x) const;
};

/** STL swap overload. */
void swap(x64asm::R& lhs, x64asm::R& rhs);

/** I/O overload. */
ostream& operator<<(ostream& os, const x64asm::Rl& r);

/** I/O overload. */
ostream& operator<<(ostream& os, const x64asm::Rh& r);

/** I/O overload. */
ostream& operator<<(ostream& os, const x64asm::Rb& r);

/** I/O overload. */
ostream& operator<<(ostream& os, const x64asm::R16& r);

/** I/O overload. */
ostream& operator<<(ostream& os, const x64asm::R32& r);

/** I/O overload. */
ostream& operator<<(ostream& os, const x64asm::R64& r);

} // namespace std

namespace x64asm {

inline R::R(const R& rhs) {
  val_ = rhs.val_;
}

inline R::R(R&& rhs) {
  val_ = rhs.val_;
}

inline R& R::operator=(const R& rhs) {
  R(rhs).swap(*this);
}

inline R& R::operator=(R&& rhs) {
  R(std::move(rhs)).swap(*this);
}

inline constexpr R::operator uint64_t() {
  return val_;
}

inline constexpr size_t R::hash() {
  return val_;
}

inline void R::swap(R& rhs) {
  val_ = rhs.val_;
}

inline constexpr R::R(uint64_t val) : 
    Operand {val} {
}

inline constexpr bool Rl::check() {
  return val_ < 4;
}

inline constexpr bool Rl::operator<(const Rl& rhs) {
  return val_ < rhs.val_;
}

inline constexpr bool Rl::operator==(const Rl& rhs) {
  return val_ == rhs.val_;
}

inline constexpr bool Rl::operator!=(const Rl& rhs) {
  return !(*this == rhs);
}

inline std::ostream& Rl::write_att(std::ostream& os) const {
  assert(check());
  const char* rls[4] = {"al","cl","dl","bl"};
  return (os << "%" << rls[val_]);
}

inline constexpr Rl::Rl(uint64_t val) : 
    R {val} {
}

inline constexpr bool Al::check() {
  return val_ == 0;
}

inline constexpr Al::Al() : 
    Rl {0} {
}

inline constexpr bool Cl::check() {
  return val_ == 1;
}

inline constexpr Cl::Cl() : 
    Rl {1} {
}

inline constexpr bool Rh::check() {
  return val_ >= 4 && val_ < 8;
}

inline constexpr bool Rh::operator<(const Rh& rhs) {
  return val_ < rhs.val_;
}

inline constexpr bool Rh::operator==(const Rh& rhs) {
  return val_ == rhs.val_;
}

inline constexpr bool Rh::operator!=(const Rh& rhs) {
  return !(*this == rhs);
}

inline std::ostream& Rh::write_att(std::ostream& os) const {
  assert(check());
  const char* rhs[4] = {"ah","ch","dh","bh"};
  return (os << "%" << rhs[val_-4]);
}

inline constexpr Rh::Rh(uint64_t val) : 
    R {val} {
}

inline constexpr bool Rb::check() {
  return val_ >= 4 && val_ < 16;
}

inline constexpr bool Rb::operator<(const Rb& rhs) {
  return val_ < rhs.val_;
}

inline constexpr bool Rb::operator==(const Rb& rhs) {
  return val_ == rhs.val_;
}

inline constexpr bool Rb::operator!=(const Rb& rhs) {
  return !(*this == rhs);
}

inline std::ostream& Rb::write_att(std::ostream& os) const {
  assert(check());
  const char* rbs[12] = {"spl","bpl","sil","dil","r8b","r9b","r10b",
      "r11b","r12b","r13b","r14b","r15b"};
  return (os << "%" << rbs[val_-4]);
}

inline constexpr Rb::Rb(uint64_t val) : 
    R {val} {
}

inline constexpr bool R16::check() {
  return val_ < 16;
}

inline constexpr bool R16::operator<(const R16& rhs) {
  return val_ < rhs.val_;
}

inline constexpr bool R16::operator==(const R16& rhs) {
  return val_ == rhs.val_;
}

inline constexpr bool R16::operator!=(const R16& rhs) {
  return !(*this == rhs);
}

inline std::ostream& R16::write_att(std::ostream& os) const {
  assert(check());
  const char* r16s[16] = {"ax","cx","dx","bx","sp","bp","si","di","r8w",
      "r9w","r10w","r11w","r12w","r13w","r14w","r15w"};
  return (os << "%" << r16s[val_]);
}

inline constexpr R16::R16(uint64_t val) : 
    R {val} {
}

inline constexpr bool Ax::check() {
  return val_ == 0;
}

inline constexpr Ax::Ax() : 
    R16 {0} {
}

inline constexpr bool Dx::check() {
  return val_ == 2;
}

inline constexpr Dx::Dx() : 
    R16 {2} {
}

inline constexpr bool R32::check() {
  return val_ < 16;
}

inline constexpr bool R32::operator<(const R32& rhs) {
  return val_ < rhs.val_;
}

inline constexpr bool R32::operator==(const R32& rhs) {
  return val_ == rhs.val_;
}

inline constexpr bool R32::operator!=(const R32& rhs) {
  return !(*this == rhs);
}

inline std::ostream& R32::write_att(std::ostream& os) const {
  assert(check());
  const char* r32s[16] = {"eax","ecx","edx","ebx","esp","ebp","esi","edi",
      "r8d","r9d","r10d","r11d","r12d","r13d","r14d","r15d"};
  return (os << "%" << r32s[val_]);
}

inline constexpr R32::R32(uint64_t val) : 
    R {val} {
}

inline constexpr bool Eax::check() {
  return val_ == 0;
}

inline constexpr Eax::Eax() : 
    R32 {0} {
}

inline constexpr bool R64::check() {
  return val_ < 16;
}

inline constexpr bool R64::operator<(const R64& rhs) {
  return val_ < rhs.val_;
}

inline constexpr bool R64::operator==(const R64& rhs) {
  return val_ == rhs.val_;
}

inline constexpr bool R64::operator!=(const R64& rhs) {
  return !(*this == rhs);
}

inline std::ostream& R64::write_att(std::ostream& os) const {
  assert(check());
  const char* r64s[16] = {"rax","rcx","rdx","rbx","rsp","rbp","rsi","rdi",
      "r8","r9","r10","r11","r12","r13","r14","r15"};
  return (os << "%" << r64s[val_]);
}

inline constexpr R64::R64(uint64_t val) : 
    R {val} {
}

inline constexpr bool Rax::check() {
  return val_ == 0;
}

inline constexpr Rax::Rax() : 
    R64 {0} {
}

} // namespace x64asm

namespace std {

inline size_t hash<x64asm::R>::operator()(const x64asm::R& r) const {
  return r.hash();
}

inline void swap(x64asm::R& lhs, x64asm::R& rhs) {
  lhs.swap(rhs);
}

inline ostream& operator<<(ostream& os, const x64asm::Rl& r) {
  return r.write_att(os);
}

inline ostream& operator<<(ostream& os, const x64asm::Rh& r) {
  return r.write_att(os);
}

inline ostream& operator<<(ostream& os, const x64asm::Rb& r) {
  return r.write_att(os);
}

inline ostream& operator<<(ostream& os, const x64asm::R16& r) {
  return r.write_att(os);
}

inline ostream& operator<<(ostream& os, const x64asm::R32& r) {
  return r.write_att(os);
}

inline ostream& operator<<(ostream& os, const x64asm::R64& r) {
  return r.write_att(os);
}

} // namespace std

#endif
