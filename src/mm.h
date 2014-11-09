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

#ifndef X64ASM_SRC_MM_H
#define X64ASM_SRC_MM_H

#include <cassert>
#include <iostream>

#include "src/operand.h"

namespace x64asm {

/** An MMX register. The 64-bit MMX registers are: MM0 through MM7. */
class Mm : public Operand {
    // Needs access to constructor.
    friend class Constants;

  public:
    /** Copy constructor. */
    Mm(const Mm& rhs) : Operand(0,0) {
			val_ = rhs.val_;
		}
    /** Move constructor. */
    Mm(Mm&& rhs) {
			val_ = rhs.val_;
		}
    /** Copy assignment operator. */
		Mm& operator=(const Mm& rhs) {
			Mm(rhs).swap(*this);
			return *this;
		}
    /** Move assignment operator. */
    Mm& operator=(Mm&& rhs) {
			Mm(std::move(rhs)).swap(*this);
			return *this;
		}

    /** Returns true if this xmm register is well-formed. */
    constexpr bool check() {
			return val_ < 8;
		}

    /** Comparison based on on val_. */
    constexpr bool operator<(const Mm& rhs) {
			return val_ < rhs.val_;
		}
    /** Comparison based on on val_. */
    constexpr bool operator==(const Mm& rhs) {
			return val_ == rhs.val_;
		}
    /** Comparison based on on val_. */
    constexpr bool operator!=(const Mm& rhs) {
			return !(*this == rhs);
		}

    /** Conversion based on underlying value. */
    constexpr operator uint64_t() {
			return val_;
		}

    /** STL-compliant hash. */
    constexpr size_t hash() {
			return val_;
		}
    /** STL-compliant swap. */
    void swap(Mm& rhs) {
  		std::swap(val_, rhs.val_);
		}

    /** Returns the type of an MMX register. */
    Type type() const { return MM; }

		/** @todo This method is undefined. */
		std::istream& read_att(std::istream& is) {
			is.setstate(std::ios::failbit);
			return is;
		}
    /** Writes this xmm register to an ostream using at&t syntax. */
    std::ostream& write_att(std::ostream& os) const {
  		assert(check());
  		return (os << "%mm" << std::dec << val_);
		}

  protected:
    /** Direct access to this constructor is disallowed. */
    constexpr Mm(uint64_t val) : Operand(val) {}
};

} // namespace x64asm

namespace std {

/** STL hash specialization. */
template <>
struct hash<x64asm::Mm> {
  size_t operator()(const x64asm::Mm& m) const {
		return m.hash();
	}
};

/** STL swap overload. */
inline void swap(x64asm::Mm& lhs, x64asm::Mm& rhs) {
	lhs.swap(rhs);
}

/** iostream overload. */
inline istream& operator>>(istream& is, x64asm::Mm& m) {
	return m.read_att(is);
}
/** iostream overload. */
inline ostream& operator<<(ostream& os, const x64asm::Mm& m) {
	return m.write_att(os);
}

} // namespace std

#endif
