#ifndef X64_SRC_ASSEMBLER_FUNCTION_H
#define X64_SRC_ASSEMBLER_FUNCTION_H

#include <sys/mman.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __APPLE__
	#define MAP_ANONYMOUS MAP_ANON
#endif

namespace x64 {

/** A JIT-assembled function. */
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
			buffer_ = (unsigned char*) mmap(0, capacity,
					PROT_READ | PROT_WRITE | PROT_EXEC,
					MAP_PRIVATE | MAP_ANONYMOUS,
					-1, 0);
			capacity_ = capacity;
		}

		inline ~Function() {
			if ( (long) buffer_ != -1 )
				munmap(buffer_, capacity_);
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

	private:
		unsigned char* buffer_;
		size_t capacity_;
};

} // namespace x64

#endif
