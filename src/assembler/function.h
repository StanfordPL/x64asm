#ifndef X64_SRC_ASSEMBLER_FUNCTION_H
#define X64_SRC_ASSEMBLER_FUNCTION_H

#include <sys/mman.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __APPLE__
	#define MAP_ANONYMOUS MAP_ANON
#endif

namespace x64 {

/** An executable Function.
*/
class Function {
	friend class Assembler;

	private:
		typedef uint64_t (*f0_type)();
		typedef uint64_t (*f1_type)(uint64_t);
		typedef uint64_t (*f2_type)(uint64_t, uint64_t);
		typedef uint64_t (*f3_type)(uint64_t, uint64_t, uint64_t z);

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
		inline uint64_t operator()(uint64_t x) {
			return ((f1_type)(buffer_))(x);
		}
		inline uint64_t operator()(uint64_t x, uint64_t y) {
			return ((f2_type)(buffer_))(x, y);
		}
		inline uint64_t operator()(uint64_t x, uint64_t y, uint64_t z) {
			return ((f3_type)(buffer_))(x, y, z);
		}

	private:
		unsigned char* buffer_;
		size_t capacity_;
};

} // namespace x64

#endif
