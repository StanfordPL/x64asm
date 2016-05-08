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

#ifndef X64ASM_ENV_BITS_H
#define X64ASM_ENV_BITS_H

#include <iostream>
#include <stdint.h>

namespace x64asm {

/** An environment register bit. */
class EnvBits {
public:
  /** Copy constructor. */
  constexpr EnvBits(const EnvBits& rhs) : index_(rhs.index_), width_(rhs.width_) {}
  /** Move constructor. */
  constexpr EnvBits(const EnvBits&& rhs) : index_(rhs.index_), width_(rhs.width_) {}
  /** Assignment operator. */
  EnvBits& operator=(const EnvBits& rhs) {
    EnvBits(rhs).swap(*this);
    return *this;
  }
  /** Move assignment operator. */
  EnvBits& operator=(const EnvBits&& rhs) {
    EnvBits(std::move(rhs)).swap(*this);
    return *this;
  }

  /** Returns this bit's upper register index. */
  constexpr size_t index() const {
    return index_;
  }
  /** Returns the number of bits this register bit spans. */
  constexpr size_t width() const {
    return width_;
  }

  /** Less than. */
  constexpr bool operator<(const EnvBits& rhs) const {
    return index_ == rhs.index_ ? width_ < rhs.width_ : index_ < rhs.index_;
  }
  /** Equality. */
  constexpr bool operator==(const EnvBits& rhs) const {
    return index_ == rhs.index_ && width_ == rhs.width_;
  }
  /** Inequality. */
  constexpr bool operator!=(const EnvBits& rhs) const {
    return !(*this == rhs);
  }

  /** STL-compliant swap. */
  void swap(EnvBits& rhs) {
    std::swap(index_, rhs.index_);
    std::swap(width_, rhs.width_);
  }

protected:
  /** Direct access to this constructor is disallowed. */
  constexpr EnvBits(size_t i, size_t w) : index_(i), width_(w) {}

  /** This bit's upper register index. */
  uint32_t index_;
  /** The number of bits this register bit spans. */
  uint32_t width_;
};

/** An EFLAGS register bit. */
class Eflags : public EnvBits {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Read from an istream using text. */
  std::istream& read_text(std::istream& is);
  /** Write to an ostream using text. */
  std::ostream& write_text(std::ostream& os) const;

private:
  /** Direct access to this constructor is disallowed. */
  constexpr Eflags(size_t i, size_t w) : EnvBits(i, w) {}
};

/** An FPU control register bit. */
class FpuControl : public EnvBits {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Read from an istream using text. */
  std::istream& read_text(std::istream& is);
  /** Write to an ostream using text. */
  std::ostream& write_text(std::ostream& os) const;

private:
  /** Direct access to this constructor is disallowed. */
  constexpr FpuControl(size_t i, size_t w) : EnvBits(i, w) {}
};

/** An FPU status register bit. */
class FpuStatus : public EnvBits {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Read from an istream using text. */
  std::istream& read_text(std::istream& is);
  /** Write to an ostream using text. */
  std::ostream& write_text(std::ostream& os) const;

private:
  /** Direct access to this constructor is disallowed. */
  constexpr FpuStatus(size_t i, size_t w) : EnvBits(i, w) {}
};

/** An FPU tag register. */
class FpuTag : public EnvBits {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Read from an istream using text. */
  std::istream& read_text(std::istream& is);
  /** Write to an ostream using text. */
  std::ostream& write_text(std::ostream& os) const;

private:
  /** Direct access to this constructor is disallowed. */
  constexpr FpuTag(size_t i, size_t w) : EnvBits(i, w) {}
};

/** An MXCSR register bit. */
class Mxcsr : public EnvBits {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Read from an istream using text. */
  std::istream& read_text(std::istream& is);
  /** Write to an ostream using text. */
  std::ostream& write_text(std::ostream& os) const;

private:
  /** Direct access to this constructor is disallowed. */
  constexpr Mxcsr(size_t i, size_t w) : EnvBits(i, w) {}
};

} // namespace std

namespace std {

/** STL-compliant swap */
inline void swap(x64asm::EnvBits& lhs, x64asm::EnvBits& rhs) {
  lhs.swap(rhs);
}

/** iostream overload */
inline istream& operator>>(istream& is, x64asm::Eflags& e) {
  return e.read_text(is);
}
/** iostream overload */
inline ostream& operator<<(ostream& os, const x64asm::Eflags& e) {
  return e.write_text(os);
}

/** iostream overload */
inline istream& operator>>(istream& is, x64asm::FpuControl& f) {
  return f.read_text(is);
}
/** iostream overload */
inline ostream& operator<<(ostream& os, const x64asm::FpuControl& f) {
  return f.write_text(os);
}

/** iostream overload */
inline istream& operator>>(istream& is, x64asm::FpuStatus& s) {
  return s.read_text(is);
}
/** iostream overload */
inline ostream& operator<<(ostream& os, const x64asm::FpuStatus& s) {
  return s.write_text(os);
}

/** iostream overload */
inline istream& operator>>(istream& is, x64asm::FpuTag& t) {
  return t.read_text(is);
}
/** iostream overload */
inline ostream& operator<<(ostream& os, const x64asm::FpuTag& t) {
  return t.write_text(os);
}

/** iostream overload */
inline istream& operator>>(istream& is, x64asm::Mxcsr& m) {
  return m.read_text(is);
}
/** iostream overload */
inline ostream& operator<<(ostream& os, const x64asm::Mxcsr& m) {
  return m.write_text(os);
}

} // namespace std

#endif

