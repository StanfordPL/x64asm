#include "src/assembler/function.h"

using namespace std;

#ifdef __APPLE__
	#define MAP_ANONYMOUS MAP_ANON
#endif

namespace x64 {

void Function::init(size_t length, 
		                size_t stack_size, size_t heap_size, uint64_t min_heap) {
	buffer_ = (unsigned char*) mmap(0, length,
			                            PROT_READ | PROT_WRITE | PROT_EXEC,
                   		            MAP_PRIVATE | MAP_ANONYMOUS,
								                  -1, 0);
	length_ = length;

	stack_ = stack_size > 0 ? new uint8_t[stack_size] : 0;
	heap_ = heap_size > 0 ? new uint8_t[heap_size] : 0;
	min_heap_ = min_heap;
}

} // namespace x64
