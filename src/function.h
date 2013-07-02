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
    // Needs access to internal buffer address.
    friend class Imm64;

  public:
    /** Returns a new function; internal buffer may be larger than specified. */
    Function(size_t capacity = 1024);
    /** Copy constructor. */
    Function(const Function& rhs);
    /** Move constructor. */
    Function(Function&& rhs);
    /** Copy assignment operator. */
    Function& operator=(const Function& rhs);
    /** Move assignment operator. */
    Function& operator=(Function&& rhs);
    /** Destructor. */
    ~Function();

    /** Returns the address of the entrypoint of this function. */
    void* get_entrypoint() const;

    /** Zero argument usage form. */
    template <typename Y>
    Y call() const;
    /** One argument usage form. */
    template <typename Y, typename X1>
    Y call(X1 x1) const;
    /** Two argument usage form. */
    template <typename Y, typename X1, typename X2>
    Y call(X1 x1, X2 x2) const;
    /** Three argument usage form. */
    template <typename Y, typename X1, typename X2, typename X3>
    Y call(X1 x1, X2 x2, X3 x3) const;
    /** Four argument usage form. */
    template <typename Y, typename X1, typename X2, typename X3,
              typename X4>
    Y call(X1 x1, X2 x2, X3 x3, X4 x4) const;
    /** Five argument usage form. */
    template <typename Y, typename X1, typename X2, typename X3,
              typename X4, typename X5>
    Y call(X1 x1, X2 x2, X3 x3, X4 x4, X5 x5) const;
    /** Six argument usage form. */
    template <typename Y, typename X1, typename X2, typename X3,
              typename X4, typename X5, typename X6>
    Y call(X1 x1, X2 x2, X3 x3, X4 x4, X5 x5, X6 x6) const;

    /** Checks that the internal buffer was allocated correctly. */
    bool good() const;

    /** Returns the number of bytes written to the internal buffer. */
    size_t size() const;
    /** Returns the total number of bytes in the internal buffer. */
    size_t capacity() const;

    /** Extends the size of the internal buffer; reallocates if necessary. */
    void reserve(size_t capacity);
    /** Resets the write pointer to the beginning of the internal pointer. */
    void clear();

    /** Emits a byte at and increments the write pointer. */
    void emit_byte(uint64_t b);
    /** Emits a byte at a user specified location. */
    void emit_byte(uint64_t b, size_t index);
    /** Emits a word and increments the write pointer. */
    void emit_word(uint64_t w);
    /** Emits a word at a user specified location. */
    void emit_word(uint64_t w, size_t index);
    /** Emits a long and increments the write pointer. */
    void emit_long(uint64_t l);
    /** Emits a long at a user specified location. */
    void emit_long(uint64_t l, size_t index);
    /** Emits a quad at and increments the write pointer. */
    void emit_quad(uint64_t q);
    /** Emits a quad at a user defined location. */
    void emit_quad(uint64_t q, size_t index);

    /** Increments the write pointer by one byte. */
    void advance_byte();
    /** Increments the write pointer by two bytes. */
    void advance_word();
    /** Increments the write pointer by four bytes. */
    void advance_long();
    /** Increments the write pointer by eight bytes. */
    void advance_quad();

    /** Equality using hex contents. */
    bool operator==(const Function& rhs) const;
    /** Equality using hex contents. */
    bool operator!=(const Function& rhs) const;
    /** Comparison using hex contents. */
    bool operator<(const Function& rhs) const;

    /** STL compliant swap. */
    void swap(Function& rhs);
    /** STL compliant hash. */
    size_t hash() const;

    /** Writes this function to an ostream in human-readable hex. */
    std::ostream& write_hex(std::ostream& os) const;

  private:
    /** The size of the internal buffer. */
    size_t capacity_;
    /** The internal buffer. */
    unsigned char* buffer_;
    /** The current write position in the internal buffer. */
    unsigned char* head_;

    /** Returns the number of bytes remaining in the internal buffer. */
    size_t remaining() const;

    /** Rounds an integer up to the nearest multiple of 1024. */
    size_t round_up(size_t size) const;
};

} // namespace x64asm

namespace std {

/** STL-swap overload. */
void swap(x64asm::Function& f1, x64asm::Function& f2);

/** STL-hash specialization. */
template <>
struct hash<x64asm::Function> {
  size_t operator()(const x64asm::Function& f) const;
};

/** I/O overload. */
ostream& operator<<(ostream& os, const x64asm::Function& f);

} // namespace std

namespace x64asm {

inline Function::Function(size_t capacity) {
  capacity_ = round_up(capacity);
  buffer_ = (unsigned char*) mmap(0, capacity_,
      PROT_READ | PROT_WRITE | PROT_EXEC,
      MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  head_ = buffer_;
}

inline Function::Function(const Function& rhs) {
  if (good())
    munmap(buffer_, capacity_);

  capacity_ = rhs.capacity_;
  buffer_ = (unsigned char*) mmap(0, capacity_,
      PROT_READ | PROT_WRITE | PROT_EXEC,
      MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (good() && rhs.good())
    memcpy(buffer_, rhs.buffer_, rhs.size());
  head_ = buffer_ + rhs.size();
}

inline Function::Function(Function&& rhs) {
  capacity_ = rhs.capacity_;
  buffer_ = rhs.buffer_;
  head_ = rhs.head_;

  rhs.buffer_ = (unsigned char*)-1;
}

inline Function& Function::operator=(const Function& rhs) {
  Function(rhs).swap(*this);
  return *this;
}

inline Function& Function::operator=(Function&& rhs) {
  Function(std::move(rhs)).swap(*this);
  return *this;
}

inline Function::~Function() {
  if (good()) {
    munmap(buffer_, capacity_);
    buffer_ = (unsigned char*)-1;
  }
}

inline void* Function::get_entrypoint() const {
  return buffer_;
}

template <typename Y>
inline Y Function::call() const {
  return ((Y(*)()) buffer_)();
}

template <typename Y, typename X1>
inline Y Function::call(X1 x1) const {
  return ((Y(*)(X1)) buffer_)(x1);
}

template <typename Y, typename X1, typename X2>
inline Y Function::call(X1 x1, X2 x2) const {
  return ((Y(*)(X1, X2)) buffer_)(x1, x2);
}

template <typename Y, typename X1, typename X2, typename X3>
inline Y Function::call(X1 x1, X2 x2, X3 x3) const {
  return ((Y(*)(X1, X2, X3)) buffer_)(x1, x2, x3);
}

template <typename Y, typename X1, typename X2, typename X3,
          typename X4>
inline Y Function::call(X1 x1, X2 x2, X3 x3, X4 x4) const {
  return ((Y(*)(X1, X2, X3, X4)) buffer_)(x1, x2, x3, x4);
}

template <typename Y, typename X1, typename X2, typename X3,
          typename X4, typename X5 >
inline Y Function::call(X1 x1, X2 x2, X3 x3, X4 x4, X5 x5) const {
  return ((Y(*)(X1, X2, X3, X4, X5)) buffer_)(x1, x2, x3, x4, x5);
}

template <typename Y, typename X1, typename X2, typename X3,
          typename X4, typename X5, typename X6 >
inline Y Function::call(X1 x1, X2 x2, X3 x3, X4 x4, X5 x5, X6 x6) const {
  return ((Y(*)(X1, X2, X3, X4, X5, X6)) buffer_)(x1, x2, x3, x4, x5, x6);
}

inline bool Function::good() const {
  return (long) buffer_ != -1;
}

inline size_t Function::size() const {
  return head_ - buffer_;
}

inline size_t Function::capacity() const {
  return capacity_;
}

inline void Function::reserve(size_t capacity) {
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

inline void Function::clear() {
  head_ = buffer_;
}

inline void Function::emit_byte(uint64_t b) {
  assert(remaining() >= 1);
  *((uint8_t*) head_) = b;
  advance_byte();
}

inline void Function::emit_byte(uint64_t b, size_t index) {
  assert(index <= size() - 1);
  buffer_[index] = b & 0xff;
}

inline void Function::emit_word(uint64_t w) {
  assert(remaining() >= 2);
  *((uint16_t*) head_) = w;
  advance_word();
}

inline void Function::emit_word(uint64_t w, size_t index) {
  assert(index <= size() - 2);
  *((uint16_t*)(buffer_ + index)) = w;
}

inline void Function::emit_long(uint64_t l) {
  assert(remaining() >= 4);
  *((uint32_t*) head_) = l;
  advance_long();
}

inline void Function::emit_long(uint64_t l, size_t index) {
  assert(index <= size() - 4);
  *((uint32_t*)(buffer_ + index)) = l;
}

inline void Function::emit_quad(uint64_t q) {
  assert(remaining() >= 8);
  *((uint64_t*) head_) = q;
  advance_quad();
}

inline void Function::emit_quad(uint64_t q, size_t index) {
  assert(index <= size() - 8);
  *((uint64_t*)(buffer_ + index)) = q;
}

inline void Function::advance_byte() {
  assert(remaining() >= 1);
  head_++;
}

inline void Function::advance_word() {
  assert(remaining() >= 2);
  head_ += 2;
}

inline void Function::advance_long() {
  assert(remaining() >= 4);
  head_ += 4;
}

inline void Function::advance_quad() {
  assert(remaining() >= 8);
  head_ += 8;
}

inline bool Function::operator==(const Function& rhs) const {
  if ( !good() || !rhs.good() )
    return false;
  if ( size() != rhs.size() )
    return false;
  return std::string((const char*)buffer_, size()) == 
      std::string((const char*)rhs.buffer_, rhs.size());
}

inline bool Function::operator!=(const Function& rhs) const {
  return !(*this == rhs);
}

inline bool Function::operator<(const Function& rhs) const {
  if ( good() != rhs.good() )
    return good() < rhs.good();
  else if ( size() != rhs.size() )
    return size() < rhs.size();
  else
    return std::string((const char*)buffer_, size()) < 
        std::string((const char*)rhs.buffer_, rhs.size());
}

inline void Function::swap(Function& rhs) {
  std::swap(capacity_, rhs.capacity_);
  std::swap(buffer_, rhs.buffer_);
  std::swap(head_, rhs.head_);
}

inline size_t Function::hash() const {
  return good() ? std::hash<std::string>()(std::string((const char*)buffer_, size())) : 0;
}

inline std::ostream& Function::write_hex(std::ostream& os) const {
  for (size_t i = 0, ie = size(); i < ie; ++i) {
    os << std::hex << std::noshowbase << std::setw(2) << std::setfill('0');
    os << (int32_t)buffer_[i] << " ";
    if (((i % 8) == 7) && ((i + 1) != ie)) {
      os << std::endl;
    }
  }
  return os;
}

inline size_t Function::remaining() const {
  return capacity() - size();
}

inline size_t Function::round_up(size_t size) const {
  if (size == 0) {
    return 1024;
  } else {
    return (((size-1) / 1024) + 1) * 1024;
  }
}

} // namespace x64asm

namespace std {

inline void swap(x64asm::Function& f1, x64asm::Function& f2) {
  f1.swap(f2);
}

inline size_t hash<x64asm::Function>::operator()(const x64asm::Function& f) const {
  return f.hash();
}

inline ostream& operator<<(ostream& os, const x64asm::Function& f) {
  return f.write_hex(os);
}

} // namespace std

#endif
