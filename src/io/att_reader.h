#ifndef X64_SRC_IO_ATT_READER_H
#define X64_SRC_IO_ATT_READER_H

#include <iostream>

#include "src/code/code.h"

namespace x64 {

class AttReader {
	public:
		static void read(std::istream& is, Code& code);
};

} // namespace x64

#endif
