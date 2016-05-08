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

#ifndef X64ASM_SRC_R_H
#define X64ASM_SRC_R_H

#include <cassert>
#include <iostream>

#include "src/operand.h"

namespace x64asm {

/** A general-purpose register. */
class R : public Operand {
public:
  /** Conversion based on underlying value. */
  constexpr operator uint64_t() const {
    return val_;
  }

  /** STL-compliant hash. */
  constexpr size_t hash() const {
    return val_;
  }

  /** Reads this register from an istream using at&t syntax. */
  std::istream& read_att(std::istream& is);
  /** Writes this register to an ostream using at&t syntax. */
  std::ostream& write_att(std::ostream& os) const;

protected:
  /** Direct access to this constructor is disallowed. */
  constexpr R(Type t, uint64_t val) : Operand(t, val) {}
};


/** One of the byte general-purpose registers: Al, CL, DL, BL, BPL, SPL, DIL and SIL; 
    or one of the byte registers (R8B - R15B) available when using REX.R and 64-bit mode.
*/
class R8 : public R {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Returns true if this register is well-formed. */
  constexpr bool check() const {
    return val_ < 16;
  }

  /** Comparison based on on val_. */
  constexpr bool operator<(const R8& rhs) const {
    return val_ < rhs.val_;
  }
  /** Comparison based on on val_. */
  constexpr bool operator==(const R8& rhs) const {
    return val_ == rhs.val_;
  }
  /** Comparison based on on val_. */
  constexpr bool operator!=(const R8& rhs) const {
    return !(*this == rhs);
  }

  /** Reads this register from an ostream using at&t syntax. */
  std::istream& read_att(std::istream& is);
  /** Writes this register to an ostream using at&t syntax. */
  std::ostream& write_att(std::ostream& os) const;

protected:
  /** Direct access to this constructor is disallowed. */
  constexpr R8(uint64_t val) : R(Type::R_8, val) {}
  constexpr R8(Type t, uint64_t val) : R(t, val) {}
};

/** The byte general-purpose register AL. */
class Al : public R8 {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Returns true if this register is well-formed. */
  constexpr bool check() const {
    return val_ == 0;
  }

private:
  /** Direct access to this constructor is disallowed. */
  constexpr Al() : R8(Type::AL, 0) {}
};

/** The byte general-purpose register CL. */
class Cl : public R8 {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Returns true if this register is well-formed. */
  constexpr bool check() const {
    return val_ == 1;
  }

private:
  /** Direct access to this constructor is disallowed. */
  constexpr Cl() : R8(Type::CL, 1) {}
};

/** One of the byte general-purpose registers: AH, CH, DH, BH. */
class Rh : public R {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Returns true if this register is well-formed. */
  constexpr bool check() const {
    return val_ >= 4 && val_ < 8;
  }

  /** Comparison based on on val_. */
  constexpr bool operator<(const Rh& rhs) const {
    return val_ < rhs.val_;
  }
  /** Comparison based on on val_. */
  constexpr bool operator==(const Rh& rhs) const {
    return val_ == rhs.val_;
  }
  /** Comparison based on on val_. */
  constexpr bool operator!=(const Rh& rhs) const {
    return !(*this == rhs);
  }

  /** Reads this register from an ostream using at&t syntax. */
  std::istream& read_att(std::istream& is);
  /** Writes this register to an ostream using at&t syntax. */
  std::ostream& write_att(std::ostream& os) const;

protected:
  /** Direct access to this constructor is disallowed. */
  constexpr Rh(uint64_t val) : R(Type::RH, val) {}
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
  constexpr bool check() const {
    return val_ < 16;
  }

  /** Comparison based on on val_. */
  constexpr bool operator<(const R16& rhs) const {
    return val_ < rhs.val_;
  }
  /** Comparison based on on val_. */
  constexpr bool operator==(const R16& rhs) const {
    return val_ == rhs.val_;
  }
  /** Comparison based on on val_. */
  constexpr bool operator!=(const R16& rhs) const {
    return !(*this == rhs);
  }

  /** Reads this register from an ostream using at&t syntax. */
  std::istream& read_att(std::istream& is);
  /** Writes this register to an ostream using at&t syntax. */
  std::ostream& write_att(std::ostream& os) const;

protected:
  /** Direct access to this constructor is disallowed. */
  constexpr R16(uint64_t val) : R(Type::R_16, val) {}
  constexpr R16(Type t, uint64_t val) : R(t, val) {}
};

/** The word general-purpose register AX. */
class Ax : public R16 {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Returns true if this register is well-formed. */
  constexpr bool check() const {
    return val_ == 0;
  }

private:
  /** Direct access to this constructor is disallowed. */
  constexpr Ax() : R16(Type::AX, 0) {}
};

/** The word general-purpose register DX. */
class Dx : public R16 {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Returns true if this register is well-formed. */
  constexpr bool check() const {
    return val_ == 2;
  }

private:
  /** Direct access to this constructor is disallowed. */
  constexpr Dx() : R16(Type::DX, 2) {}
};

/** One of the doubleword general-purpose registers: EAX, ECX, EDX, EBX, ESP,
    EBP, ESI, EDI; or one of the doubleword registers (R8D - R15D) available
    when using REX.R in 64-bit mode.
*/
class R32 : public R {
  // Needs access to constructor.
  friend class Constants;
  // Needs access to consturctor.
  template <class T>
  friend class M;
  friend class Mem;

public:
  /** Returns true if this register is well-formed. */
  constexpr bool check() const {
    return val_ < 16;
  }

  /** Comparison based on on val_. */
  constexpr bool operator<(const R32& rhs) const {
    return val_ < rhs.val_;
  }
  /** Comparison based on on val_. */
  constexpr bool operator==(const R32& rhs) const {
    return val_ == rhs.val_;
  }
  /** Comparison based on on val_. */
  constexpr bool operator!=(const R32& rhs) const {
    return !(*this == rhs);
  }

  /** Reads this register from an ostream using at&t syntax. */
  std::istream& read_att(std::istream& is);
  /** Writes this register to an ostream using at&t syntax. */
  std::ostream& write_att(std::ostream& os) const;

protected:
  /** Direct access to this constructor is disallowed. */
  constexpr R32(uint64_t val) : R(Type::R_32, val) {}
  constexpr R32(Type t, uint64_t val) : R(t, val) {}
};

/** The doubleword general-purpose register EAX. */
class Eax : public R32 {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Returns true if this register is well-formed. */
  constexpr bool check() const {
    return val_ == 0;
  }

private:
  /** Direct access to this constructor is disallowed. */
  constexpr Eax() : R32(Type::EAX, 0) {}
};

/** One of the quadword general-purpose registers: RAX, RBX, RCX, RDX, RDI, RSI,
    RBP, RSP, R8–R15. These are available when using REX.R and 64-bit mode.
*/
class R64 : public R {
  // Needs access to constructor.
  friend class Constants;
  // Needs access to consturctor.
  template <class T>
  friend class M;
  friend class Mem;

public:
  /** Returns true if this register is well-formed. */
  constexpr bool check() const {
    return val_ < 16;
  }

  /** Comparison based on on val_. */
  constexpr bool operator<(const R64& rhs) const {
    return val_ < rhs.val_;
  }
  /** Comparison based on on val_. */
  constexpr bool operator==(const R64& rhs) const {
    return val_ == rhs.val_;
  }
  /** Comparison based on on val_. */
  constexpr bool operator!=(const R64& rhs) const {
    return !(*this == rhs);
  }

  /** Reads this register from an ostream using at&t syntax. */
  std::istream& read_att(std::istream& is);
  /** Writes this register to an ostream using at&t syntax. */
  std::ostream& write_att(std::ostream& os) const;

protected:
  /** Direct access to this constructor is disallowed. */
  constexpr R64(uint64_t val) : R(Type::R_64, val) {}
  constexpr R64(Type t, uint64_t val) : R(t, val) {}
};

/** The quadword general-purpose register RAX. */
class Rax : public R64 {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Returns true if this register is well-formed. */
  constexpr bool check() const {
    return val_ == 0;
  }

private:
  /** Direct access to this constructor is disallowed. */
  constexpr Rax() : R64(Type::RAX, 0) {}
};

} // namespace x64asm

namespace std {

/** STL hash specialization. */
template <>
struct hash<x64asm::R> {
  size_t operator()(const x64asm::R& r) const {
    return r.hash();
  }
};

/** iostream overload. */
inline istream& operator>>(istream& is, x64asm::Rh& r) {
  return r.read_att(is);
}
/** iostream overload. */
inline ostream& operator<<(ostream& os, const x64asm::Rh& r) {
  return r.write_att(os);
}

/** iostream overload. */
inline istream& operator>>(istream& is, x64asm::R8& r) {
  return r.read_att(is);
}
/** iostream overload. */
inline ostream& operator<<(ostream& os, const x64asm::R8& r) {
  return r.write_att(os);
}

/** iostream overload. */
inline istream& operator>>(istream& is, x64asm::R16& r) {
  return r.read_att(is);
}
/** iostream overload. */
inline ostream& operator<<(ostream& os, const x64asm::R16& r) {
  return r.write_att(os);
}

/** iostream overload. */
inline istream& operator>>(istream& is, x64asm::R32& r) {
  return r.read_att(is);
}
/** iostream overload. */
inline ostream& operator<<(ostream& os, const x64asm::R32& r) {
  return r.write_att(os);
}

/** iostream overload. */
inline istream& operator>>(istream& is, x64asm::R64& r) {
  return r.read_att(is);
}
/** iostream overload. */
inline ostream& operator<<(ostream& os, const x64asm::R64& r) {
  return r.write_att(os);
}

/** iostream overload. */
inline istream& operator<<(istream& os, x64asm::R& r) {
  return r.read_att(os);
}
/** iostream overload. */
inline ostream& operator<<(ostream& os, const x64asm::R& r) {
  return r.write_att(os);
}

} // namespace std

#endif
