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

#ifndef X64ASM_SRC_SSE_H
#define X64ASM_SRC_SSE_H

#include <cassert>
#include <iostream>

#include "src/operand.h"

namespace x64asm {

/** A SSE register. Either Ymm or Xmm (for now...) */
class Sse : public Operand {

public:
  /** Conversion based on underlying value. */
  constexpr operator uint64_t() const {
    return val_;
  }

  /** Writes this ymm register to an ostream using at&t syntax. */
  std::ostream& write_att(std::ostream& os) const;

protected:
  constexpr Sse(Type t, uint64_t val) : Operand(t, val) {}
};

} // namespace x64asm

namespace std {

/** iostream overload. */
inline ostream& operator<<(ostream& os, const x64asm::Sse& s) {
  return s.write_att(os);
}

} // namespace std

#endif
