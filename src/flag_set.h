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

#ifndef X64ASM_SRC_FLAG_SET_H
#define X64ASM_SRC_FLAG_SET_H

#include "src/flag.h"

#include <iostream>

/** A compact representation of cpuid feature flag sets. */
namespace x64asm {

class FlagSet {
private:
  /** Creates a flag set from a bit mask. */
  constexpr FlagSet(uint64_t mask) : mask_(mask) { }

public:
  /** Creates an empty flag set. */
  constexpr FlagSet() : mask_(0) {}
  /** Creates a flag set with a single element. */
  constexpr FlagSet(Flag f) : mask_((uint64_t)f) {}

  /** Copy constructor. */
  constexpr FlagSet(const FlagSet& rhs) : mask_(rhs.mask_) { }
  /** Move constructor. */
  FlagSet(FlagSet&& rhs) {
    mask_ = rhs.mask_;
  }
  /** Copy assignment operator. */
  FlagSet& operator=(const FlagSet& rhs) {
    FlagSet(rhs).swap(*this);
    return *this;
  }
  /** Move assignment operator. */
  FlagSet& operator=(FlagSet&& rhs) {
    FlagSet(std::move(rhs)).swap(*this);
    return *this;
  }

  /** Creates an empty flag set. */
  static constexpr FlagSet empty() {
    return FlagSet();
  }
  /** Creates a full flag set. */
  static constexpr FlagSet universe() {
    return FlagSet((Flag)-1);
  }

  /** Inserts a flag into this set. */
  constexpr FlagSet operator+(Flag f) const {
    return FlagSet((Flag)(mask_ | (uint64_t)f));
  }
  /** Removes a flag from this set. */
  constexpr FlagSet operator-(Flag f) const {
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
  constexpr FlagSet operator|(FlagSet rhs) const {
    return {mask_ | rhs.mask_};
  }
  /** Set difference. */
  constexpr FlagSet operator-(FlagSet rhs) const {
    return {mask_ & ~rhs.mask_};
  }
  /** Set intersection. */
  constexpr FlagSet operator&(FlagSet rhs) const {
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
  constexpr bool contains(Flag f) const {
    return ((uint64_t)f & mask_) == (uint64_t)f;
  }
  /** Does this set subsume another set? */
  constexpr bool contains(FlagSet fs) const {
    return ((uint64_t)fs.mask_ & mask_) == (uint64_t)fs.mask_;
  }
  /** Do these sets intersect? */
  constexpr bool intersects(FlagSet fs) const {
    return (*this - fs) != *this;
  }

  /** Equality based on underlying bit mask. */
  constexpr bool operator==(FlagSet rhs) const {
    return mask_ == rhs.mask_;
  }
  /** Equality based on underlying bit mask. */
  constexpr bool operator!=(FlagSet rhs) const {
    return mask_ != rhs.mask_;
  }
  /** Equality based on underlying bit mask. */
  constexpr bool operator<(FlagSet rhs) const {
    return mask_ < rhs.mask_;
  }

  /** STL compliant hash. */
  constexpr size_t hash() const {
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
  /** Underlying bitmask. */
  uint64_t mask_;
};

} // namespace x64asm

namespace std {

/** STL hash specialization. */
template <>
struct hash<x64asm::FlagSet> {
  size_t operator()(const x64asm::FlagSet& fs) const {
    return fs.hash();
  }
};

/** STL swap overload. */
inline void swap(x64asm::FlagSet& lhs, x64asm::FlagSet& rhs) {
  lhs.swap(rhs);
}

/** iostream overload. */
inline istream& operator>>(istream& is, x64asm::FlagSet& fs) {
  return fs.read_text(is);
}
/** iostream overload. */
inline ostream& operator<<(ostream& os, const x64asm::FlagSet& fs) {
  return fs.write_text(os);
}

} // namespace std

#endif
