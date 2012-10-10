#ifndef X64_SRC_CODE_READER_H
#define X64_SRC_CODE_READER_H

#include <iostream>

#include "src/code/code.h"

namespace x64 {

class Reader {
	public:
		void read_att(std::istream& is, Code& code) const;
};

} // namespace x64

#endif
