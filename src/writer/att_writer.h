#ifndef X64_SRC_WRITER_ATT_WRITER_H
#define X64_SRC_WRITER_ATT_WRITER_H

#include <iostream>

#include "src/code/cr.h"
#include "src/code/dr.h"
#include "src/code/eflag.h"
#include "src/code/imm.h"
#include "src/code/label.h"
#include "src/code/mm.h"
#include "src/code/moffs.h"

namespace x64 {

struct AttWriter {
	static void write(std::ostream& os, Cr c);
	static void write(std::ostream& os, Dr d);
	static void write(std::ostream& os, Eflag e);
	static void write(std::ostream& os, Imm i);
	static void write(std::ostream& os, Label l);
	static void write(std::ostream& os, Mm m);
	static void write(std::ostream& os, Moffs m);
};

} // namespace x64

#endif

