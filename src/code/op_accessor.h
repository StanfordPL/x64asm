#ifndef X64_SRC_ATTRIBUTES_OP_ACCESSOR_H
#define X64_SRC_ATTRIBUTES_OP_ACCESSOR_H

namespace x64 {

enum class OpAccessor {
	READ = 0,
	WRITE,
	READ_WRITE,
	NONE
};

} // namespace x64

#endif
