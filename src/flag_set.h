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

#ifndef X64ASM_SRC_FLAG_SET_H
#define X64ASM_SRC_FLAG_SET_H

#include "src/flag.h"

#include <iostream>

/** A compact representation of cpuid feature flag sets. */
namespace x64asm {

class FlagSet {
public:
  /** Creates an empty flag set. */
  constexpr FlagSet() : mask_(0) {}
  /** Creates a flag set with a single element. */
  constexpr FlagSet(Flag f) : mask_((uint64_t)f) {}

  /** Creates an empty flag set. */
  static constexpr FlagSet empty() {
    return FlagSet();
  }
  /** Creates a full flag set. */
  static constexpr FlagSet universe() {
    return FlagSet((Flag)-1);
  }

  /** Inserts a flag into this set. */
  constexpr FlagSet operator+(Flag f) {
    return FlagSet((Flag)(mask_ | (uint64_t)f));
  }
  /** Removes a flag from this set. */
  constexpr FlagSet operator-(Flag f) {
    return FlagSet((Flag)(mask_ & ~(uint64_t)f));
  }

  /** Inserts a flag into this set. */
  FlagSet& operator+=(Flag f) {
    mask_ |= (uint64_t)f;
    return *this;
  }
  /** Removes a flag from this set. */
  FlagSet& operator-=(Flag f) {
    mask_ &= ~(uint64_t)f;
    return *this;
  }

  /** Set union. */
  constexpr FlagSet operator|(FlagSet rhs) {
    return {mask_ | rhs.mask_};
  }
  /** Set difference. */
  constexpr FlagSet operator-(FlagSet rhs) {
    return {mask_ & ~rhs.mask_};
  }
  /** Set intersection. */
  constexpr FlagSet operator&(FlagSet rhs) {
    return {mask_ & rhs.mask_};
  }
  /** Set union. */
  FlagSet& operator|=(FlagSet rhs) {
    mask_ |= rhs.mask_;
    return *this;
  }
  /** Set difference. */
  FlagSet& operator-=(FlagSet rhs) {
    mask_ &= ~rhs.mask_;
    return *this;
  }
  /** Set intersection. */
  FlagSet& operator&=(FlagSet rhs) {
    mask_ &= rhs.mask_;
    return *this;
  }

  /** Does this set contain a flag? */
  constexpr bool contains(Flag f) {
    return ((uint64_t)f & mask_) == (uint64_t)f;
  }
  /** Does this set subsume another set? */
  constexpr bool contains(FlagSet fs) {
    return ((uint64_t)fs.mask_ & mask_) == (uint64_t)fs.mask_;
  }
  /** Do these sets intersect? */
  constexpr bool intersects(FlagSet fs) {
    return (*this - fs) != *this;
  }

  /** Comparison operator. */
  constexpr bool operator==(FlagSet rhs) {
    return mask_ == rhs.mask_;
  }
  /** Comparison operator. */
  constexpr bool operator!=(FlagSet rhs) {
    return mask_ != rhs.mask_;
  }
  /** Comparison operator. */
  constexpr bool operator<(FlagSet rhs) {
    return mask_ < rhs.mask_;
  }

  /** STL compliant hash. */
  constexpr size_t hash() {
    return mask_;
  }
  /** STL compliant swap. */
  void swap(FlagSet& rhs) {
    std::swap(mask_, rhs.mask_);
  }

  /** Reads a flag set from an istream in text. */
  std::istream& read_text(std::istream& is);
  /** Reads a flag set to an ostream in text. */
  std::ostream& write_text(std::ostream& os) const;

private:
  /** Creates a flag set from a bit mask. */
  constexpr FlagSet(uint64_t mask) : mask_(mask) { }

  /** Underlying bitmask. */
  uint64_t mask_;
};

} // namespace x64asm

#endif
