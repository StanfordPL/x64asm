#ifndef X64_SRC_WRITER_ATT_WRITER_H
#define X64_SRC_WRITER_ATT_WRITER_H

#include <iostream>

#include "src/code/cr.h"
#include "src/code/dr.h"
#include "src/code/eflag.h"
#include "src/code/imm.h"

namespace x64 {

struct AttWriter {
	static void write(std::ostream& os, Cr c);
	static void write(std::ostream& os, Dr d);
	static void write(std::ostream& os, Eflag e);
	static void write(std::ostream& os, Imm i);
};

} // namespace x64

#endif

