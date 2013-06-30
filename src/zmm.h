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

#ifndef X64ASM_SRC_ZMM_H
#define X64ASM_SRC_ZMM_H

#include <cassert>
#include <iostream>

#include "src/operand.h"

namespace x64asm {

/** A ZMM register. The 512-bit ZMM registers are: ZMM0 through ZMM7; ZMM8
	  through ZMM15 are available using REX.R in 64-bit mode.
*/
class Zmm : public Operand {
    // Needs access to constructor.
    friend class Constants;

  public:
    /** Returns true if this xmm register is well-formed. */
    constexpr bool check();

    /** Comparison based on underlying value. */
    constexpr bool operator<(const Zmm& rhs);
    /** Comparison based on underlying value. */
    constexpr bool operator==(const Zmm& rhs);
		/** Comparison based on underlying value. */
		constexpr bool operator!=(const Zmm& rhs);

    /** Conversion based on underlying value. */
    constexpr operator uint64_t();

    /** Writes this xmm register to an ostream using at&t syntax. */
    void write_att(std::ostream& os) const;

		/** STL-compliant hash. */
		constexpr size_t hash();
		/** STL-compliant swap. */
		void swap(Zmm& rhs);

  protected:
    /** Direct access to this constructor is disallowed. */
    constexpr Zmm(uint64_t val);
};

} // namespace x64asm

namespace std {

template <>
struct hash<x64asm::Zmm> {
	size_t operator()(const x64asm::Zmm& z) const;
};

template <>
void swap(x64asm::Zmm& lhs, x64asm::Zmm& rhs);

} // namespace std

namespace x64asm {

inline constexpr bool Zmm::check() {
	return val_ < 16;
}

inline constexpr bool Zmm::operator<(const Zmm& rhs) {
	assert(check() && rhs.check());
	return val_ < rhs.val_;
}

inline constexpr bool Zmm::operator==(const Zmm& rhs) {
	assert(check() && rhs.check());
	return val_ == rhs.val_;
}

inline constexpr bool Zmm::operator!=(const Zmm& rhs) {
	assert(check() && rhs.check());
	return val_ != rhs.val_;
}

inline constexpr Zmm::operator uint64_t() {
	assert(check());
	return val_;
}

inline void Zmm::write_att(std::ostream& os) const {
	assert(check());
	os << "%zmm" << std::dec << val_;
}

inline constexpr size_t Zmm::hash() {
	assert(check());
	return val_;
}

inline void Zmm::swap(Zmm& rhs) {
	assert(check() && rhs.check());
	std::swap(val_, rhs.val_);
}

inline constexpr Zmm::Zmm(uint64_t val) : Operand{val} {
	assert(check());
}

} // namespace x64asm

namespace std {

template<>
inline size_t hash<x64asm::Zmm>::operator()(const x64asm::Zmm& z) const {
	return z.hash();
}

template <>
inline void swap(x64asm::Zmm& lhs, x64asm::Zmm& rhs) {
	lhs.swap(rhs);
}

} // namespace std

#endif
