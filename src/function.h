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
#include <iostream>
#include <sys/mman.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __APPLE__
	#define MAP_ANONYMOUS MAP_ANON
#endif

namespace x64asm {

/** An executable buffer. */
class Function {
	friend class Assembler;

	private:
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
		Function(size_t capacity = 1024) {
			capacity_ = round_up(capacity);
			buffer_ = make_buffer(capacity_);
			head_ = buffer_;
		}

		Function(const Function& rhs) {
			copy_buffer(rhs);
			head_ = buffer_ + size();
		}

		Function& operator=(const Function& rhs) {
			copy_buffer(rhs);
			head_ = buffer_ + size();
			return *this;
		}

		~Function() {
			free_buffer();
		}

		uint64_t operator()() {
			return ((f0_type)(buffer_))();
		}

		template <typename RDI>
		uint64_t operator()(RDI rdi) {
			return ((f1_type)(buffer_))((uint64_t)rdi);
		}

		template <typename RDI, typename RSI>
		uint64_t operator()(RDI rdi, RSI rsi) {
			return ((f2_type)(buffer_))((uint64_t)rdi, (uint64_t)rsi);
		}

		template <typename RDI, typename RSI, typename RDX_>
		uint64_t operator()(RDI rdi, RSI rsi, RDX_ rdx) {
			return ((f3_type)(buffer_))((uint64_t)rdi, (uint64_t)rsi, (uint64_t)rdx);
		}

		template <typename RDI, typename RSI, typename RDX_,
						  typename RCX>
		uint64_t operator()(RDI rdi, RSI rsi, RDX_ rdx, RCX rcx) {
			return ((f4_type)(buffer_))((uint64_t)rdi, (uint64_t)rsi, (uint64_t)rdx,
					                        (uint64_t)rcx);
		}

		template <typename RDI, typename RSI, typename RDX_,
						  typename RCX, typename R8>
		uint64_t operator()(RDI rdi, RSI rsi, RDX_ rdx, RCX rcx, R8 r8) {
			return ((f5_type)(buffer_))((uint64_t)rdi, (uint64_t)rsi, (uint64_t)rdx,
					                        (uint64_t)rcx, (uint64_t)r8);
		}

		template <typename RDI, typename RSI, typename RDX_,
						  typename RCX, typename R8, typename R9>
		uint64_t operator()(RDI rdi, RSI rsi, RDX_ rdx, RCX rcx, R8 r8, R9 r9) {
			return ((f6_type)(buffer_))((uint64_t)rdi, (uint64_t)rsi, (uint64_t)rdx,
					                        (uint64_t)rcx, (uint64_t)r8,  (uint64_t)r9);
		}

		bool good() const {
			return (long) buffer_ != -1;
		}

	private:
		size_t capacity_;
		unsigned char* buffer_;
		unsigned char* head_;

		size_t round_up(size_t size) const {
			if ( size == 0 )
				return 1024;
			else if ( size % 1024 == 0 )
				return size;
			else
				return ((size/1024)+1) * 1024;
		}

		unsigned char* make_buffer(size_t size) const {
			return (unsigned char*) mmap(0, size,
					PROT_READ | PROT_WRITE | PROT_EXEC,
					MAP_PRIVATE | MAP_ANONYMOUS,
					-1, 0);
		}

		void copy_buffer(const Function& rhs) {
			capacity_ = rhs.capacity_;
			buffer_ = make_buffer(rhs.capacity_);
			if ( good() )
				memcpy(buffer_, rhs.buffer_, rhs.size());	
		}

		void free_buffer() {
			if ( good() )
				munmap(buffer_, capacity_);
		}

		size_t size() const {
			return head_ - buffer_;
		}

		size_t capacity() const {
			return capacity_;
		}

		void resize(size_t capacity) {
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

		size_t remaining() const {
			return capacity() - size();
		}

		void clear() {
			head_ = buffer_;
		}

		void emit_byte(uint64_t b) {
			assert(remaining() >= 1);
			*((uint8_t*) head_) = b;
			head_++;
		}

		void emit_byte(uint64_t b, size_t index) {
			assert(index <= size() - 1);
			buffer_[index] = b & 0xff;
		}

		void emit_word(uint64_t w) {
			assert(remaining() >= 2);
			*((uint16_t*) head_) = w;
			head_ += 2;
		}

		void emit_word(uint64_t w, size_t index) {
			assert(index <= size() - 2);
			*((uint16_t*) (buffer_ + index)) = w;
		}

		void emit_long(uint64_t l) {
			assert(remaining() >= 4);
			*((uint32_t*) head_) = l;
			head_ += 4;
		}

		void emit_long(uint64_t l, size_t index) {
			assert(index <= size() - 4);
			*((uint32_t*) (head_ + index)) = l;
		}

		void emit_quad(uint64_t q) {
			assert(remaining() >= 8);
			*((uint64_t*) head_) = q;
			head_ += 8;
		}

		void emit_quad(uint64_t q, size_t index) {
			assert(index <= size() - 8);
			*((uint64_t*) (head_ + index)) = q;
		}

		void advance_byte() {
			assert(remaining() >= 1);
			head_++;
		}

		void advance_word() {
			assert(remaining() >= 2);
			head_ += 2;
		}

		void advance_long() {
			assert(remaining() >= 4);
			head_ += 4;
		}

		void advance_quad() {
			assert(remaining() >= 8);
			head_ += 8;
		}
};

} // namespace x64asm

#endif
