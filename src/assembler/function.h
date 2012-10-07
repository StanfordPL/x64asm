#ifndef X64_SRC_ASSEMBLER_FUNCTION_H
#define X64_SRC_ASSEMBLER_FUNCTION_H

#include "src/assembler/assembler.h"

#include <sys/mman.h>
#include <stddef.h>
#include <stdint.h>

namespace x64 {

/** An executable Function with an associated sandbox.
*/
class Function {
	friend class Assembler;

	private:
		typedef uint64_t (*f0_type)();
		typedef uint64_t (*f1_type)(uint64_t);
		typedef uint64_t (*f2_type)(uint64_t, uint64_t);
		typedef uint64_t (*f3_type)(uint64_t, uint64_t, uint64_t z);

	public:
		inline Function() {
			init(1024, 0, 0, 0);
		}

		inline Function(size_t stack_size, size_t heap_size, uint64_t min_heap) {
			init(1024, stack_size, heap_size, min_heap);
		}

		inline ~Function() {
			if ( (long) buffer_ != -1 )
				munmap(buffer_, length_);
			if ( stack_ != 0 )
				delete stack_;
			if ( heap_ != 0 )
				delete heap_;
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
		size_t length_;

		uint8_t* stack_;
		uint8_t* heap_;
		uint64_t min_heap_;

		void init(size_t length, 
				      size_t stack_size, size_t heap_size, uint64_t min_heap);
};

} // namespace x64

#endif
