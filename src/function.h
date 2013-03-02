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

/** An executable buffer. Supports zero to six argument calling conventions.
    In general, a function can be called with arguments of any type which can
    be implicitly converted to uint64_ts. Virtually all of the methods in 
		this class are private. Direct access is granted only to the assembler
		class, which is (in theory) guaranteed to be sound. 
*/
class Function {
	// Needs access to the private API.
	friend class Assembler;
	// Needs access to internal buffer address.
	friend class Imm64;

	private:
		// Convenience typedefs for different calling conventions.
		typedef uint64_t (*f0_type)();
		typedef uint64_t (*f1_type)(uint64_t);
		typedef uint64_t (*f2_type)(uint64_t, uint64_t);
		typedef uint64_t (*f3_type)(uint64_t, uint64_t, uint64_t);
		typedef uint64_t (*f4_type)(uint64_t, uint64_t, uint64_t,
				                        uint64_t);
		typedef uint64_t (*f5_type)(uint64_t, uint64_t, uint64_t,
				                        uint64_t, uint64_t);
		typedef uint64_t (*f6_type)(uint64_t, uint64_t, uint64_t,
				                        uint64_t, uint64_t, uint64_t);

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

		/** Zero argument usage form. */
		uint64_t operator()() const {
			return ((f0_type)(buffer_))();
		}

		/** One argument usage form. */
		template <typename RDI>
		uint64_t operator()(RDI rdi) const {
			return ((f1_type)(buffer_))((uint64_t)rdi);
		}

		/** Two argument usage form. */
		template <typename RDI, typename RSI>
		uint64_t operator()(RDI rdi, RSI rsi) const {
			return ((f2_type)(buffer_))((uint64_t)rdi, (uint64_t)rsi);
		}

		/** Three argument usage form. */
		template <typename RDI, typename RSI, typename RDX_>
		uint64_t operator()(RDI rdi, RSI rsi, RDX_ rdx) const {
			return ((f3_type)(buffer_))((uint64_t)rdi, (uint64_t)rsi, (uint64_t)rdx);
		}

		/** Four argument usage form. */
		template <typename RDI, typename RSI, typename RDX_,
						  typename RCX>
		uint64_t operator()(RDI rdi, RSI rsi, RDX_ rdx, RCX rcx) const {
			return ((f4_type)(buffer_))((uint64_t)rdi, (uint64_t)rsi, (uint64_t)rdx,
					                        (uint64_t)rcx);
		}

		/** Five argument usage form. */
		template <typename RDI, typename RSI, typename RDX_,
						  typename RCX, typename R8>
		uint64_t operator()(RDI rdi, RSI rsi, RDX_ rdx, RCX rcx, R8 r8) const {
			return ((f5_type)(buffer_))((uint64_t)rdi, (uint64_t)rsi, (uint64_t)rdx,
					                        (uint64_t)rcx, (uint64_t)r8);
		}

		/** Six argument usage form. */
		template <typename RDI, typename RSI, typename RDX_,
						  typename RCX, typename R8, typename R9>
		uint64_t operator()(RDI rdi, RSI rsi, RDX_ rdx, RCX rcx, R8 r8, R9 r9) const {
			return ((f6_type)(buffer_))((uint64_t)rdi, (uint64_t)rsi, (uint64_t)rdx,
					                        (uint64_t)rcx, (uint64_t)r8,  (uint64_t)r9);
		}

		/** Returns true iff the internal buffer associated with this function was
			  allocated correctly.  Does NOT guarantee that this function does 
				something safe or sensible.  That's on you.
		*/
		bool good() const {
			return (long) buffer_ != -1;
		}

		/** Writes this function to an ostream in human-readable hex. */
		void write_hex(std::ostream& os) const {
			for ( size_t i = 0, ie = size(); i < ie; ++i ) {
				os << std::hex << std::noshowbase << std::setw(2) << std::setfill('0');
				os << (int32_t)buffer_[i] << " ";
				if ( ((i%8) == 7) && ((i+1) != ie) )
					os << std::endl;
			}
		}

	private:
		/** The size of the internal buffer. */
		size_t capacity_;
		/** The internal buffer. */
		unsigned char* buffer_;
		/** The current write position in the internal buffer. */
		unsigned char* head_;

		/** Rounds an integer up to the nearest multiple of 1024. */
		size_t round_up(size_t size) const {
			if ( size == 0 )
				return 1024;
			else if ( size % 1024 == 0 )
				return size;
			else
				return ((size/1024)+1) * 1024;
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
			if ( good() )
				memcpy(buffer_, rhs.buffer_, rhs.size());	
		}

		/** Deallocates a buffer. */
		void free_buffer() {
			if ( good() )
				munmap(buffer_, capacity_);
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
			if ( capacity <= capacity_ )
				return;

			capacity = round_up(capacity);
			auto buf = make_buffer(capacity);
			if ( (long) buf != -1 )
				memcpy(buf, buffer_, size());

			free_buffer();
			capacity_ = capacity;
			buffer_ = buf;
		}

		/** Returns the number of bytes remaining in the internal buffer. */
		size_t remaining() const {
			return capacity() - size();
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
			*((uint16_t*) (buffer_ + index)) = w;
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
			*((uint32_t*) (buffer_ + index)) = l;
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
			*((uint64_t*) (buffer_ + index)) = q;
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
};

} // namespace x64asm

#endif
