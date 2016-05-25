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

#ifndef X64ASM_SRC_FUNCTION_H
#define X64ASM_SRC_FUNCTION_H

#include <algorithm>
#include <cassert>
#include <cstring>
#include <functional>
#include <iomanip>
#include <iostream>
#include <sys/mman.h>
#include <stddef.h>
#include <stdint.h>
#include <string>
#include <unordered_map>
#include <vector>

#ifdef __APPLE__
#define MAP_ANONYMOUS MAP_ANON
#endif

namespace x64asm {

/** An executable hex buffer. Supports zero to six argument calling
    conventions. In general, a function can be called with arguments of any
    type which are or can be implicitly converted to native types.
*/
class Function {
  // Needs access to internal buffer.
  friend class Assembler;
  // Needs access to label data.
  friend class Linker;

public:
  /** Returns a new function; internal buffer may be larger than specified. */
  Function(size_t capacity = 1024) {
    capacity_ = round_up(capacity);
    buffer_ = (unsigned char*) mmap(0, capacity_,
                                    PROT_READ | PROT_WRITE | PROT_EXEC,
                                    MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    head_ = buffer_;
  }
  /** Copy constructor. */
  Function(const Function& rhs) : buffer_((unsigned char*)-1) {
    if (good())
      munmap(buffer_, capacity_);

    capacity_ = rhs.capacity_;
    buffer_ = (unsigned char*) mmap(0, capacity_,
                                    PROT_READ | PROT_WRITE | PROT_EXEC,
                                    MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (good() && rhs.good())
      memcpy(buffer_, rhs.buffer_, rhs.size());
    head_ = buffer_ + rhs.size();

    label_defs_ = rhs.label_defs_;
    label8_rels_ = rhs.label8_rels_;
    label32_rels_ = rhs.label32_rels_;
  }
  /** Move constructor. */
  Function(Function&& rhs) {
    capacity_ = rhs.capacity_;
    buffer_ = rhs.buffer_;
    head_ = rhs.head_;

    label_defs_ = std::move(rhs.label_defs_);
    label8_rels_ = std::move(rhs.label8_rels_);
    label32_rels_ = std::move(rhs.label32_rels_);

    rhs.buffer_ = (unsigned char*)-1;
  }

  /** Copy assignment operator. */
  Function& operator=(const Function& rhs) {
    Function(rhs).swap(*this);
    return *this;
  }
  /** Move assignment operator. */
  Function& operator=(Function&& rhs) {
    Function(std::move(rhs)).swap(*this);
    return *this;
  }

  /** Destructor. */
  ~Function() {
    if (good()) {
      munmap(buffer_, capacity_);
      buffer_ = (unsigned char*)-1;
    }
  }

  /** Returns a pointer to the internal buffer */
  void* data() const {
    return buffer_;
  }
  /** Returns the address of the entrypoint of this function. */
  void* get_entrypoint() const {
    return data();
  }

  /** Zero argument usage form. */
  template <typename Y>
  Y call() const {
    return ((Y(*)()) buffer_)();
  }
  /** One argument usage form. */
  template <typename Y, typename X1>
  Y call(X1 x1) const {
    return ((Y(*)(X1)) buffer_)(x1);
  }
  /** Two argument usage form. */
  template <typename Y, typename X1, typename X2>
  Y call(X1 x1, X2 x2) const {
    return ((Y(*)(X1, X2)) buffer_)(x1, x2);
  }
  /** Three argument usage form. */
  template <typename Y, typename X1, typename X2, typename X3>
  Y call(X1 x1, X2 x2, X3 x3) const {
    return ((Y(*)(X1, X2, X3)) buffer_)(x1, x2, x3);
  }
  /** Four argument usage form. */
  template <typename Y, typename X1, typename X2, typename X3,
            typename X4>
  Y call(X1 x1, X2 x2, X3 x3, X4 x4) const {
    return ((Y(*)(X1, X2, X3, X4)) buffer_)(x1, x2, x3, x4);
  }
  /** Five argument usage form. */
  template <typename Y, typename X1, typename X2, typename X3,
            typename X4, typename X5>
  Y call(X1 x1, X2 x2, X3 x3, X4 x4, X5 x5) const {
    return ((Y(*)(X1, X2, X3, X4, X5)) buffer_)(x1, x2, x3, x4, x5);
  }
  /** Six argument usage form. */
  template <typename Y, typename X1, typename X2, typename X3,
            typename X4, typename X5, typename X6>
  Y call(X1 x1, X2 x2, X3 x3, X4 x4, X5 x5, X6 x6) const {
    return ((Y(*)(X1, X2, X3, X4, X5, X6)) buffer_)(x1, x2, x3, x4, x5, x6);
  }

  /** Returns the number of bytes written to the internal buffer. */
  size_t size() const {
    return head_ - buffer_;
  }
  /** Returns the total number of bytes in the internal buffer. */
  size_t capacity() const {
    return capacity_;
  }

  /** Extends the size of the internal buffer; reallocates if necessary. */
  void reserve(size_t capacity) {
    if (capacity <= capacity_) {
      return;
    }

    Function rhs(capacity);
    if ( good() && rhs.good() ) {
      memcpy(rhs.buffer_, buffer_, capacity_);
      rhs.head_ = rhs.buffer_ + size();
    }

    rhs.swap(*this);
  }
  /** Resets the write pointer to the beginning of the internal pointer. */
  void clear() {
    head_ = buffer_;
    label_defs_.clear();
    label8_rels_.clear();
    label32_rels_.clear();
  }

  /** Emits a byte at and increments the write pointer. */
  void emit_byte(uint64_t b) {
    assert(remaining() >= 1);
    *((uint8_t*) head_) = b;
    advance_byte();
  }
  /** Emits a byte at a user specified location. */
  void emit_byte(uint64_t b, size_t index) {
    assert(index <= size() - 1);
    buffer_[index] = b & 0xff;
  }
  /** Emits a word and increments the write pointer. */
  void emit_word(uint64_t w) {
    assert(remaining() >= 2);
    *((uint16_t*) head_) = w;
    advance_word();
  }
  /** Emits a word at a user specified location. */
  void emit_word(uint64_t w, size_t index) {
    assert(index <= size() - 2);
    *((uint16_t*)(buffer_ + index)) = w;
  }
  /** Emits a long and increments the write pointer. */
  void emit_long(uint64_t l) {
    assert(remaining() >= 4);
    *((uint32_t*) head_) = l;
    advance_long();
  }
  /** Emits a long at a user specified location. */
  void emit_long(uint64_t l, size_t index) {
    assert(index <= size() - 4);
    *((uint32_t*)(buffer_ + index)) = l;
  }
  /** Emits a quad at and increments the write pointer. */
  void emit_quad(uint64_t q) {
    assert(remaining() >= 8);
    *((uint64_t*) head_) = q;
    advance_quad();
  }
  /** Emits a quad at a user defined location. */
  void emit_quad(uint64_t q, size_t index) {
    assert(index <= size() - 8);
    *((uint64_t*)(buffer_ + index)) = q;
  }

  /** Increments the write pointer by one byte. */
  void advance_byte() {
    assert(remaining() >= 1);
    head_++;
  }
  /** Increments the write pointer by two bytes. */
  void advance_word() {
    assert(remaining() >= 2);
    head_ += 2;
  }
  /** Increments the write pointer by four bytes. */
  void advance_long() {
    assert(remaining() >= 4);
    head_ += 4;
  }
  /** Increments the write pointer by eight bytes. */
  void advance_quad() {
    assert(remaining() >= 8);
    head_ += 8;
  }

  /** Checks that the internal buffer was allocated correctly. */
  bool good() const {
    return (long) buffer_ != -1;
  }

  /** Equality using hex contents. */
  bool operator==(const Function& rhs) const {
    if ( !good() || !rhs.good() )
      return false;
    if ( size() != rhs.size() )
      return false;
    return std::string((const char*)buffer_, size()) ==
           std::string((const char*)rhs.buffer_, rhs.size());
  }
  /** Equality using hex contents. */
  bool operator!=(const Function& rhs) const {
    return !(*this == rhs);
  }
  /** Comparison using hex contents. */
  bool operator<(const Function& rhs) const {
    if ( good() != rhs.good() )
      return good() < rhs.good();
    else if ( size() != rhs.size() )
      return size() < rhs.size();
    else
      return std::string((const char*)buffer_, size()) <
             std::string((const char*)rhs.buffer_, rhs.size());
  }

  /** STL compliant swap. */
  void swap(Function& rhs) {
    std::swap(capacity_, rhs.capacity_);
    std::swap(buffer_, rhs.buffer_);
    std::swap(head_, rhs.head_);
  }
  /** STL compliant hash. */
  size_t hash() const {
    return good() ? std::hash<std::string>()(std::string((const char*)buffer_, size())) : 0;
  }

  /** @todo This method is undefined. */
  std::istream& read_hex(std::istream& is) {
    is.setstate(std::ios::failbit);
    return is;
  }
  /** Writes this function to an ostream in human-readable hex. */
  std::ostream& write_hex(std::ostream& os) const {
    const auto fmt = os.flags();
    for (size_t i = 0, ie = size(); i < ie; ++i) {
      os << std::hex << std::noshowbase << std::setw(2) << std::setfill('0');
      os << (int32_t)buffer_[i] << " ";
      if (((i % 8) == 7) && ((i + 1) != ie)) {
        os << std::endl;
      }
    }
    os.flags(fmt);
    return os;
  }

  /** Deprecated: Use data() */
  unsigned char* get_buffer() {
    return buffer_;
  }
  /** Deprecated: Use data() + size() */
  unsigned char* get_head() {
    return head_;
  }

private:
  /** The size of the internal buffer. */
  size_t capacity_;
  /** The internal buffer. */
  unsigned char* buffer_;
  /** The current write position in the internal buffer. */
  unsigned char* head_;

  /** Maps label definitions to code position. */
  std::unordered_map<uint64_t, size_t> label_defs_;
  /** Keeps track of unresolved 8-bit label references. */
  std::vector<std::pair<size_t, uint64_t>> label8_rels_;
  /** Keeps track of unresolved 32-bit label references. */
  std::vector<std::pair<size_t, uint64_t>> label32_rels_;

  /** Returns the number of bytes remaining in the internal buffer. */
  size_t remaining() const {
    return capacity() - size();
  }

  /** Rounds an integer up to the nearest multiple of 1024. */
  size_t round_up(size_t size) const {
    if (size == 0) {
      return 1024;
    } else {
      return (((size-1) / 1024) + 1) * 1024;
    }
  }
};

} // namespace x64asm

namespace std {

/** STL-swap overload. */
inline void swap(x64asm::Function& f1, x64asm::Function& f2) {
  f1.swap(f2);
}

/** STL-hash specialization. */
template <>
struct hash<x64asm::Function> {
  size_t operator()(const x64asm::Function& f) const {
    return f.hash();
  }
};

/** iostream overload. */
inline istream& operator>>(istream& is, x64asm::Function& f) {
  return f.read_hex(is);
}
inline ostream& operator<<(ostream& os, const x64asm::Function& f) {
  return f.write_hex(os);
}

} // namespace std

#endif
