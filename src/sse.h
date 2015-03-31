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

#ifndef X64ASM_SRC_SSE_H
#define X64ASM_SRC_SSE_H

#include <iostream>

#include "src/operand.h"

namespace x64asm {

/** A SSE register. Either Ymm or Xmm (for now...) */
class Sse : public Operand {

public:
  /** Returns true if this xmm register is well-formed. */
  constexpr bool check() {
    return val() < 16;
  }

  /** Conversion based on underlying value. */
  constexpr operator uint64_t() {
    return val();
  }

  /** Writes this ymm register to an ostream using at&t syntax. */
  std::ostream& write_att(std::ostream& os) const;

protected:
  constexpr Sse(Type t, uint64_t val) : Operand(t, val) {}
};

/** An XMM register. The 128-bit XMM registers are: XMM0 through XMM7; XMM8
    through XMM15 are available using REX.R in 64-bit mode.
*/
class Xmm : public Sse {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Reads this xmm register from an ostream using at&t syntax. */
  std::istream& read_att(std::istream& is);
  /** Writes this xmm register to an ostream using at&t syntax. */
  std::ostream& write_att(std::ostream& os) const;

protected:
  /** Direct access to this constructor is disallowed. */
  constexpr Xmm(uint64_t val) : Sse(Type::XMM, val) {}
  constexpr Xmm(Type t, uint64_t val) : Sse(t, val) {}
};

/** The XMM register XMM0. */
class Xmm0 : public Xmm {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Returns true if this xmm register is %xmm0. */
  constexpr bool check() {
    return val() == 0;
  }

private:
  /** Direct access to this constructor is disallowed. */
  constexpr Xmm0() : Xmm(Type::XMM_0, 0) {}
};

/** A YMM register. The 256-bit YMM registers are: YMM0 through YMM7; YMM8
    through YMM15 are available using REX.R in 64-bit mode.
*/
class Ymm : public Sse {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Reads this ymm register from an ostream using at&t syntax. */
  std::istream& read_att(std::istream& is);
  /** Writes this ymm register to an ostream using at&t syntax. */
  std::ostream& write_att(std::ostream& os) const;

protected:
  /** Direct access to this constructor is disallowed. */
  constexpr Ymm(uint64_t val) : Sse(Type::YMM, val) {}
};

} // namespace x64asm

#endif
