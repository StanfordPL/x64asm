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

#ifndef X64ASM_SRC_ALIAS_H
#define X64ASM_SRC_ALIAS_H

#include <cassert>
#include <type_traits>

#include "src/constants.h"
#include "src/mm.h"
#include "src/r.h"
#include "src/st.h"
#include "src/type_traits.h"
#include "src/xmm.h"
#include "src/ymm.h"

namespace x64asm {

/** A static class for resolving hardware aliasing relationships. */
class Alias {
public:
  /** Convert a general purpose register to its high alias.
      This method is undefined when no alias exists (eg: %r12).
  */
  template <typename T>
  static typename std::enable_if <is_reg<T>::value && !std::is_same<T, Rh>::value, const Rh>::type
  to_high(const T& t) {
    assert(t < 4);
    return Constants::rhs()[t];
  }

  /** Convert a general purpose register to its high alias.
      This method is undefined for byte registers and when no alias
      exists (ie: %r12).
  */
  static const Rh to_high(const Rh& r) {
    return r;
  }

  /** Convert a general purpose register to its byte alias. */
  template <typename T>
  static typename std::enable_if<is_reg<T>::value && !std::is_same<T, Rh>::value, const R8>::type
  to_byte(const T& t) {
    return Constants::r8s()[t];
  }

  /** Convert a general purpose register to its byte alias. */
  static const R8 to_byte(const Rh& r) {
    return Constants::r8s()[r-4];
  }

  /** Convert a general purpose register to its word alias. */
  template <typename T>
  static typename std::enable_if <is_reg<T>::value && !std::is_same<T, Rh>::value, const R16>::type
  to_word(const T& t) {
    return Constants::r16s()[t];
  }

  /** Convert a general purpose register to its word alias. */
  static const R16 to_word(const Rh& r) {
    // High registers are assigned internal values beginning from 4.
    return Constants::r16s()[r - 4];
  }

  /** Convert a general purpose register to its double alias. */
  template <typename T>
  static typename std::enable_if<is_reg<T>::value && !std::is_same<T, Rh>::value, const R32>::type
  to_double(const T& t) {
    return Constants::r32s()[t];
  }

  /** Convert a general purpose register to its double alias. */
  static const R32 to_double(const Rh& r) {
    // High registers are assigned internal values beginning from 4.
    return Constants::r32s()[r - 4];
  }

  /** Convert a general purpose register to its quad alias. */
  template <typename T>
  static typename std::enable_if <is_reg<T>::value && !std::is_same<T, Rh>::value, const R64>::type
  to_quad(const T& t) {
    return Constants::r64s()[t];
  }

  /** Convert a general purpose register to its quad alias. */
  static const R64 to_quad(const Rh& r) {
    // High registers are assigned internal values beginning from 4.
    return Constants::r64s()[r - 4];
  }

  /** Convert an element of the floating point stack to its mmx alias.
      Results are dependent on the value of the top pointer.
  */
  static const Mm to_mm(const St& st, const St& top) {
    return Constants::mms()[(st + top) % 8];
  }

  /** Convert an mmx register to its floating point stack alias.
      Results are dependent on the value of the top pointer.
  */
  static const St to_st(const Mm& mm, const St& top) {
    return Constants::sts()[(mm + top) % 8];
  }

  /** Converts a ymm register to its xmm alias. */
  static const Xmm to_xmm(const Ymm& y) {
    return Constants::xmms()[y];
  }

  /** Converts an xmm register to its ymm alias. */
  static const Ymm to_ymm(const Xmm& x) {
    return Constants::ymms()[x];
  }
};

} // namespace x64asm

#endif

