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

#ifndef X64ASM_SRC_HINT_H
#define X64ASM_SRC_HINT_H

#include <cassert>
#include <iostream>

#include "src/operand.h"

namespace x64asm {

/** A taken/not-taken hint for conditional jumps. */
class Hint : public Operand {
    // Needs access to constructor.
    friend class Constants;

  public:
    /** Copy constructor. */
    Hint(const Hint& rhs) : Operand(0,0) {
			val_ = rhs.val_;
		}
    /** Move constructor. */
    Hint(Hint&& rhs) {
			val_ = rhs.val_;
		}
    /** Copy assignment operator. */
    Hint& operator=(const Hint& rhs) {
			Hint(rhs).swap(*this);
			return *this;
		}
    /** Move assignment operator. */
    Hint& operator=(Hint&& rhs) {
			Hint(std::move(rhs)).swap(*this);
			return *this;
		}

    /** Checks that this hint is well-formed. */
    constexpr bool check() {
			return val_ < 2;
		}

    /** Comparison based on val_. */
    constexpr bool operator==(const Hint& rhs) {
			return val_ == rhs.val_;
		}
    /** Comparison based on val_. */
    constexpr bool operator!=(const Hint& rhs) {
			return !(*this == rhs);
		}
    /** Comparison based on val_. */
    constexpr bool operator<(const Hint& rhs) {
			return val_ < rhs.val_;
		}

    /** Conversion based on val_. */
    constexpr operator uint64_t() {
			return val_;
		}

    /** STL-compliant hash. */
    constexpr size_t hash() {
			return val_;
		}
    /** STL-compliant swap. */
    void swap(Hint& rhs) {
			std::swap(val_, rhs.val_);
		}

    /** Writes this hint to an ostream using (something like) at&t syntax. */
		std::ostream& write_att(std::ostream& os) const {
			assert(check());
			return (os << (val_ == 0 ? "<taken>" : "<not taken>"));
		}

  private:
    /** Direct access to this constructor is disallowed. */
    constexpr Hint(uint64_t val) : Operand(val) {}
};

} // namespace x64asm

namespace std {

/** STL hash specialization. */
template <>
struct hash<x64asm::Hint> {
  size_t operator()(const x64asm::Hint& h) const {
		return h.hash();
	}
};

/** STL swap overload. */
inline void swap(x64asm::Hint& lhs, x64asm::Hint& rhs) {
	lhs.swap(rhs);
}

/** I/O overload. */
inline ostream& operator<<(ostream& os, const x64asm::Hint& h) {
	return h.write_att(os);
}

} // namespace std

#endif
