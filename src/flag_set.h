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

/** A compact representation of cpuid feature flag sets. */
namespace x64asm {

class FlagSet {
  public:
    /** Creates an empty flag set. */
    constexpr FlagSet();
    /** Creates a flag set with a single element. */
    constexpr FlagSet(Flag f);

    /** Creates an empty flag set. */
    static constexpr FlagSet empty();
    /** Creates a full flag set. */
    static constexpr FlagSet universe();

    /** Inserts a flag into this set. */
    constexpr FlagSet operator+(Flag f);
    /** Removes a flag from this set. */
    constexpr FlagSet operator-(Flag f);

    /** Inserts a flag into this set. */
    FlagSet& operator+=(Flag f);
    /** Removes a flag from this set. */
    FlagSet& operator-=(Flag f);

    /** Does this set contain a flag? */
    constexpr bool contains(Flag f);
    /** Does this set subsume another set? */
    constexpr bool contains(FlagSet fs);

    /** Equality based on underlying bit mask. */
    constexpr bool operator==(FlagSet rhs);
    /** Equality based on underlying bit mask. */
    constexpr bool operator!=(FlagSet rhs);
    /** Equality based on underlying bit mask. */
    constexpr bool operator<(FlagSet rhs);

    /** STL compliant hash. */
    constexpr size_t hash();
    /** STL compliant swap. */
    void swap(FlagSet rhs);

  private:
    /** Underlying bitmask. */
    uint64_t mask_;  
};

} // namespace x64asm

namespace std {

/** STL hash specialization. */
template <>
struct hash<x64asm::FlagSet> {
  size_t operator()(const x64asm::FlagSet& fs) const;
};

/** STL swap overload. */
void swap(x64asm::FlagSet& lhs, x64asm::FlagSet& rhs);

} // namespace std

namespace x64asm {

inline constexpr FlagSet::FlagSet() :
    mask_{0} {
}

inline constexpr FlagSet::FlagSet(Flag f) :
    mask_{(uint64_t)f} {
}

inline constexpr FlagSet FlagSet::empty() {
  return FlagSet();
}

inline constexpr FlagSet FlagSet::universe() {
  return FlagSet((Flag)-1);
}

inline constexpr FlagSet FlagSet::operator+(Flag f) {
  return FlagSet{(Flag)(mask_ | (uint64_t)f)};
}

inline constexpr FlagSet FlagSet::operator-(Flag f) {
  return FlagSet{(Flag)(mask_ & ~(uint64_t)f)};
}

inline FlagSet& FlagSet::operator+=(Flag f) {
  mask_ |= (uint64_t)f;
  return *this;
}

inline FlagSet& FlagSet::operator-=(Flag f) {
  mask_ &= ~(uint64_t)f;
  return *this;
}

inline constexpr bool FlagSet::contains(Flag f) {
  return (uint64_t)f & mask_ == (uint64_t)f;
}

inline constexpr bool FlagSet::contains(FlagSet fs) {
  return (uint64_t)fs.mask_ & mask_ == (uint64_t)fs.mask_;
}

inline constexpr bool FlagSet::operator==(FlagSet rhs) {
  return mask_ == rhs.mask_;
}

inline constexpr bool FlagSet::operator!=(FlagSet rhs) {
  return mask_ != rhs.mask_;
}

inline constexpr bool FlagSet::operator<(FlagSet rhs) {
  return mask_ < rhs.mask_;
}

inline constexpr size_t FlagSet::hash() {
  return mask_;
}

inline void FlagSet::swap(FlagSet rhs) {
  std::swap(mask_, rhs.mask_);
}

} // namespace x64asm

namespace std {

inline size_t hash<x64asm::FlagSet>::operator()(const x64asm::FlagSet& fs) const {
  return fs.hash();
}

inline void swap(x64asm::FlagSet& lhs, x64asm::FlagSet& rhs) {
  lhs.swap(rhs);
}

} // namespace std

#endif
