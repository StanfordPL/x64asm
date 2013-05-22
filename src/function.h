/*
Copyright 2103 eric schkufza

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

#include <cassert>
#include <cstring>
#include <iomanip>
#include <iostream>
#include <sys/mman.h>
#include <stddef.h>
#include <stdint.h>

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
    /** Returns a new function with a default 1k internal buffer.
    	  The internal buffer may be larger than the value specified by
    		a non-default argument.
    */
    Function(size_t capacity = 1024) {
      capacity_ = round_up(capacity);
      buffer_ = make_buffer(capacity_);
      head_ = buffer_;
    }

    /** Deep copy constructor. */
    Function(const Function& rhs) {
      copy_buffer(rhs);
      head_ = buffer_ + size();
    }

    /** Deep copy assignment operator. */
    Function& operator=(const Function& rhs) {
      copy_buffer(rhs);
      head_ = buffer_ + size();
      return *this;
    }

    /** Deallocates the internal buffer. */
    ~Function() {
      free_buffer();
    }

		/** Returns the address of the entrypoint of this function. */
		void* get_entrypoint() const {
			return buffer_;
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
    template < typename Y, typename X1, typename X2, typename X3,
             typename X4 >
    Y call(X1 x1, X2 x2, X3 x3, X4 x4) const {
      return ((Y(*)(X1, X2, X3, X4)) buffer_)(x1, x2, x3, x4);
    }

    /** Five argument usage form. */
    template < typename Y, typename X1, typename X2, typename X3,
             typename X4, typename X5 >
    Y call(X1 x1, X2 x2, X3 x3, X4 x4, X5 x5) const {
      return ((Y(*)(X1, X2, X3, X4, X5)) buffer_)(x1, x2, x3, x4, x5);
    }

    /** Six argument usage form. */
    template < typename Y, typename X1, typename X2, typename X3,
             typename X4, typename X5, typename X6 >
    Y call(X1 x1, X2 x2, X3 x3, X4 x4, X5 x5, X6 x6) const {
      return ((Y(*)(X1, X2, X3, X4, X5, X6)) buffer_)(x1, x2, x3, x4, x5, x6);
    }

    /** Returns true iff the internal buffer associated with this function was
    	  allocated correctly.  Does NOT guarantee that this function does
    		something safe or sensible.  That's up to you.
    */
    bool good() const {
      return (long) buffer_ != -1;
    }

    /** Returns the number of bytes written to the internal buffer. */
    size_t size() const {
      return head_ - buffer_;
    }

    /** Returns the total number of bytes in the internal buffer. */
    size_t capacity() const {
      return capacity_;
    }

    /** Extends the size of the internal buffer; performsa a reallocation if
    	  necessary.
    */
    void reserve(size_t capacity) {
      if (capacity <= capacity_) {
        return;
      }

      capacity = round_up(capacity);
      auto buf = make_buffer(capacity);
      if ((long) buf != -1) {
        memcpy(buf, buffer_, size());
      }

      free_buffer();
      capacity_ = capacity;
      buffer_ = buf;
    }

    /** Resets the write pointer to the beginning of the internal pointer. */
    void clear() {
      head_ = buffer_;
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

    /** Writes this function to an ostream in human-readable hex. */
    void write_hex(std::ostream& os) const {
      for (size_t i = 0, ie = size(); i < ie; ++i) {
        os << std::hex << std::noshowbase << std::setw(2) << std::setfill('0');
        os << (int32_t)buffer_[i] << " ";
        if (((i % 8) == 7) && ((i + 1) != ie)) {
          os << std::endl;
        }
      }
    }

  private:
    /** The size of the internal buffer. */
    size_t capacity_;
    /** The internal buffer. */
    unsigned char* buffer_;
    /** The current write position in the internal buffer. */
    unsigned char* head_;

    /** Returns the number of bytes remaining in the internal buffer. */
    size_t remaining() const {
      return capacity() - size();
    }

    /** Rounds an integer up to the nearest multiple of 1024. */
    size_t round_up(size_t size) const {
      if (size == 0) {
        return 1024;
      } else if (size % 1024 == 0) {
        return size;
      } else {
        return ((size / 1024) + 1) * 1024;
      }
    }

    /** Allocates an executable buffer of at least size bytes. */
    unsigned char* make_buffer(size_t size) const {
      return (unsigned char*) mmap(0, size,
                                   PROT_READ | PROT_WRITE | PROT_EXEC,
                                   MAP_PRIVATE | MAP_ANONYMOUS,
                                   -1, 0);
    }

    /** Performs a deep copy of a buffer. */
    void copy_buffer(const Function& rhs) {
      capacity_ = rhs.capacity_;
      buffer_ = make_buffer(rhs.capacity_);
      if (good()) {
        memcpy(buffer_, rhs.buffer_, rhs.size());
      }
    }

    /** Deallocates a buffer. */
    void free_buffer() {
      if (good()) {
        munmap(buffer_, capacity_);
      }
    }
};

} // namespace x64asm

#endif
