#ifndef X64_SRC_ATT_ATT_READER_H
#define X64_SRC_ATT_ATT_READER_H

#include <iostream>

#include "src/code/code.h"

namespace x64 {

class AttReader {
	public:
		void read(std::istream& is, Code& code) const;
};

} // namespace x64

#endif
