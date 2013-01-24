#ifndef X64_SRC_ASSEMBLER_FUNCTION_H
#define X64_SRC_ASSEMBLER_FUNCTION_H

#include <cassert>
#include <cstring>
#include <iostream>
#include <sys/mman.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __APPLE__
	#define MAP_ANONYMOUS MAP_ANON
#endif

namespace x64 {

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
		inline Function(size_t capacity = 1024) {
			capacity_ = round_up(capacity);
			buffer_ = make_buffer(capacity_);
			head_ = buffer_;
		}

		inline Function(const Function& rhs) {
			copy_buffer(rhs);
			head_ = buffer_ + size();
		}

		inline Function& operator=(const Function& rhs) {
			copy_buffer(rhs);
			head_ = buffer_ + size();
			return *this;
		}

		inline ~Function() {
			free_buffer();
		}

		inline uint64_t operator()() {
			return ((f0_type)(buffer_))();
		}
		inline uint64_t operator()(uint64_t rdi) {
			return ((f1_type)(buffer_))(rdi);
		}
		inline uint64_t operator()(uint64_t rdi, uint64_t rsi) {
			return ((f2_type)(buffer_))(rdi, rsi);
		}
		inline uint64_t operator()(uint64_t rdi, uint64_t rsi, uint64_t rdx) {
			return ((f3_type)(buffer_))(rdi, rsi, rdx);
		}
		inline uint64_t operator()(uint64_t rdi, uint64_t rsi, uint64_t rdx,
				                       uint64_t rcx) {
			return ((f4_type)(buffer_))(rdi, rsi, rdx, rcx);
		}
		inline uint64_t operator()(uint64_t rdi, uint64_t rsi, uint64_t rdx,
				                       uint64_t rcx, uint64_t r8) {
			return ((f5_type)(buffer_))(rdi, rsi, rdx, rcx, r8);
		}
		inline uint64_t operator()(uint64_t rdi, uint64_t rsi, uint64_t rdx,
				                       uint64_t rcx, uint64_t r8,  uint64_t r9) {
			return ((f6_type)(buffer_))(rdi, rsi, rdx, rcx, r8, r9);
		}

		inline bool is_executable() const {
			return (long) buffer_ != -1;
		}

		inline size_t size() const {
			return head_ - buffer_;
		}

		inline size_t capacity() const {
			return capacity_;
		}

		inline void resize(size_t capacity) {
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

	private:
		size_t capacity_;
		unsigned char* buffer_;
		unsigned char* head_;

		inline size_t round_up(size_t size) const {
			if ( size == 0 )
				return 1024;
			else if ( size % 1024 == 0 )
				return size;
			else
				return ((size/1024)+1) * 1024;
		}

		inline unsigned char* make_buffer(size_t size) const {
			return (unsigned char*) mmap(0, size,
					PROT_READ | PROT_WRITE | PROT_EXEC,
					MAP_PRIVATE | MAP_ANONYMOUS,
					-1, 0);
		}

		inline void copy_buffer(const Function& rhs) {
			capacity_ = rhs.capacity_;
			buffer_ = make_buffer(rhs.capacity_);
			if ( is_executable() )
				memcpy(buffer_, rhs.buffer_, rhs.size());	
		}

		inline void free_buffer() {
			if ( is_executable() )
				munmap(buffer_, capacity_);
		}

		inline size_t remaining() const {
			return capacity() - size();
		}

		inline void clear() {
			head_ = buffer_;
		}

		inline void emit_byte(uint64_t b) {
			assert(remaining() >= 1);
			*((uint8_t*) head_) = b;
			head_++;
		}

		inline void emit_byte(uint64_t b, size_t index) {
			assert(index <= size() - 1);
			buffer_[index] = b & 0xff;
		}

		inline void emit_word(uint64_t w) {
			assert(remaining() >= 2);
			*((uint16_t*) head_) = w;
			head_ += 2;
		}

		inline void emit_word(uint64_t w, size_t index) {
			assert(index <= size() - 2);
			*((uint16_t*) (buffer_ + index)) = w;
		}

		inline void emit_long(uint64_t l) {
			assert(remaining() >= 4);
			*((uint32_t*) head_) = l;
			head_ += 4;
		}

		inline void emit_long(uint64_t l, size_t index) {
			assert(index <= size() - 4);
			*((uint32_t*) (head_ + index)) = l;
		}

		inline void emit_quad(uint64_t q) {
			assert(remaining() >= 8);
			*((uint64_t*) head_) = q;
			head_ += 8;
		}

		inline void emit_quad(uint64_t q, size_t index) {
			assert(index <= size() - 8);
			*((uint64_t*) (head_ + index)) = q;
		}

		inline void advance_byte() {
			assert(remaining() >= 1);
			head_++;
		}

		inline void advance_word() {
			assert(remaining() >= 2);
			head_ += 2;
		}

		inline void advance_long() {
			assert(remaining() >= 4);
			head_ += 4;
		}

		inline void advance_quad() {
			assert(remaining() >= 8);
			head_ += 8;
		}
};

} // namespace x64

#endif
