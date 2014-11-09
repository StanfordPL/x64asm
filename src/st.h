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

#ifndef X64ASM_SRC_ST_H
#define X64ASM_SRC_ST_H

#include <iostream>

#include "src/operand.h"

namespace x64asm {

/** The ith element from the top of the FPU register stack
	  (i = 0 through 7).
*/
class St : public Operand {
    // Needs access to constructor.
    friend class Constants;

  public:
    /** Copy constructor. */
    St(const St& rhs) : Operand(0,0) {
			val_ = rhs.val_;
		}
    /** Move constructor. */
    St(St&& rhs) {
			val_ = rhs.val_;
		}
    /** Copy assignment operator. */
    St& operator=(const St& rhs) {
			St(rhs).swap(*this);
			return *this;
		}
    /** Move assignment operator. */
    St& operator=(St&& rhs) {
			St(std::move(rhs)).swap(*this);
			return *this;
		}

    /** Returns true if this stack register is well-formed. */
    constexpr bool check() {
			return val_ < 8;
		}

    /** Comparison based on on val_. */
    constexpr bool operator<(const St& rhs) {
			return val_ < rhs.val_;
		}
    /** Comparison based on on val_. */
    constexpr bool operator==(const St& rhs) {
			return val_ == rhs.val_;
		}
    /** Comparison based on on val_. */
    constexpr bool operator!=(const St& rhs) {
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
    void swap(St& rhs) {
			std::swap(val_, rhs.val_);
		}

    /** Return the type of this operand */
    virtual Type type() const { return ST; }

		/** @todo This method is undefined. */
		std::istream& read_att(std::istream& is) {
			is.setstate(std::ios::failbit);
			return is;
		}
    /** Writes this stack register to an ostream using at&t syntax. */
    std::ostream& write_att(std::ostream& os) const {
			assert(check());
			os << "%";
			if (val_ == 0) {
				os << "st";
			} else {
				os << "st(" << std::dec << val_ << ")";
			}
			return os;
		}

  protected:
    /** Direct access to this constructor is disallowed. */
    constexpr St(uint64_t val) : Operand(val) {}
};

/** The top element of the FPU register stack. */
class St0 : public St {
    // Needs access to constructor.
    friend class Constants;

  public:
    /** Returns true if this stack register is %st(0). */
    constexpr bool check() {
			return val_ == 0;
		}

    /** Return the type of this operand */
    Type type() const { return ST_0; }

  private:
    /** Direct access to this constructor is disallowed. */
    constexpr St0() : St(0) {}
};

} // namespace x64asm

namespace std {

/** STL hash specialization. */
template <>
struct hash<x64asm::St> {
  size_t operator()(const x64asm::St& s) const {
		return s.hash();
	}
};

/** STL swap overload. */
inline void swap(x64asm::St& lhs, x64asm::St& rhs) {
	lhs.swap(rhs);
}

/** iostream overload. */
inline istream& operator>>(istream& is, x64asm::St& s) {
	return s.read_att(is);
}
/** iostream overload. */
inline ostream& operator<<(ostream& os, const x64asm::St& s) {
	return s.write_att(os);
}

} // namespace std

#endif
