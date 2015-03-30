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

#ifndef X64ASM_SRC_OPERAND_H
#define X64ASM_SRC_OPERAND_H

#include <array>
#include <cassert>
#include <stdint.h>
#include <utility>

#include "src/type.h"

namespace x64asm {

/** Base operand type. This class is provisioned with enough storage space
    for an operand of any type. This prevents object slicing from losing
    information if an object is cast back and form to/from an Operand.
 */
class Operand : private std::pair<uint64_t, uint64_t> {
  // Needs access to default constructor.
  friend class std::array<Operand, 4>;
  // Needs access to underlying value.
  friend class Assembler;
  // Needs access to non-default constructor.
  friend class Instruction;

  /** Mask constants */
  static constexpr uint64_t type_mask_ = 0xff;
  /** Index constants */
  static constexpr size_t type_idx_ = 3;

public:
  /** Return the type of this operand */
  constexpr Type type() {
    return (Type)val2(type_mask_, type_idx_);
  }
  /** Return the size of this operand */
  uint16_t size() const;
  /** Is this a general purpose register? */
  bool is_gp_register() const;
  /** Is this an SSE register? */
  bool is_sse_register() const;
  /** Is this a normal 8/16/32/64/128/256-bit memory operand? */
  bool is_typical_memory() const;
  /** Is this an immediate? */
  bool is_immediate() const;

  /** Comparison */
  constexpr bool operator==(const Operand& rhs) {
    return this->first == rhs.first && this->second == rhs.second;
  }
  /** Comparison */
  constexpr bool operator!=(const Operand& rhs) {
    return !(*this == rhs);
  }
  /** Comparison */
  constexpr bool operator<(const Operand& rhs) {
    return (this->first < rhs.first) ? true :
           (this->first == rhs.first)  ? false :
           this->second < rhs.second;
  }

  /** STL-compliant hash */
  constexpr size_t hash() {
    return this->first ^ this->second; 
  }
  /** STL-compliant swap */
  void swap(Operand& rhs) {
    std::pair<uint64_t, uint64_t>::swap(rhs);
  }

  /** @todo this method is unsupported */
  std::istream& read_att(std::istream& is) {
    assert(false);
    return is;
  }
  /** Write this operator to an output stream */
  std::ostream& write_att(std::ostream& os) const;

protected:
  /** Creates an operand with no type and no underlying value. */
  constexpr Operand() : std::pair<uint64_t, uint64_t>(0,0) {}
  /** Creates an operand with a type and no underlying value. */
  constexpr Operand(Type t) : std::pair<uint64_t, uint64_t>(0, (uint64_t)t << type_idx_) {}
  /** Creates an operand with a type and one underlying value. */
  constexpr Operand(Type t, uint64_t val) : 
    std::pair<uint64_t, uint64_t>(val, (uint64_t)t << type_idx_) {}
  /** Creates an operand with a type and two underlying values. */
  constexpr Operand(Type t, uint64_t val, uint64_t val2) : 
    std::pair<uint64_t, uint64_t>(val, val2 | ((uint64_t)t << type_idx_)) {}

  /** Member access */
  constexpr uint64_t val(uint64_t mask = -1ull, size_t idx = 0) {
    return (this->first >> idx) & mask;
  }
  /** Member update */
  void set_val(uint64_t val, uint64_t mask = -1ull, size_t idx = 0) {
    const auto m = mask << idx;
    this->first &= ~m;
    this->first |= (val << idx);
  }
  /** Member access */
  constexpr uint64_t val2(uint64_t mask = -1ull, size_t idx = 0) {
    return (this->second >> idx) & mask;
  }
  /** Member update */
  void set_val2(uint64_t val, uint64_t mask = -1ull, size_t idx = 0) {
    const auto m = mask << idx;
    this->second &= ~m;
    this->second |= (val << idx);
  }

private:
  /** Forcibly change the underlying type */
  void set_type(Type t) {
    set_val2((uint64_t)t, type_mask_, type_idx_);
  }
};

} // namespace x64asm

namespace std {

/** STL hash specialization. */
template <>
struct hash<x64asm::Operand> {
  size_t operator()(const x64asm::Operand& o) const {
    return o.hash();
  }
};

/** STL swap specialization */
inline void swap(x64asm::Operand& o1, x64asm::Operand& o2) {
  o1.swap(o2);
}

/** iostream overload. */
inline istream& operator>>(istream& is, x64asm::Operand& op) {
  return op.read_att(is);
}

/** iostream overload. */
inline ostream& operator<<(ostream& os, const x64asm::Operand& op) {
  return op.write_att(os);
}

} // namespace std

#endif
