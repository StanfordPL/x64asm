#ifndef X64_SRC_IO_ATT_WRITER_H
#define X64_SRC_IO_ATT_WRITER_H

#include <iostream>

#include "src/operands/cr.h"
#include "src/operands/dr.h"
#include "src/operands/eflag.h"
#include "src/operands/imm.h"
#include "src/operands/label.h"
#include "src/operands/mm.h"
#include "src/operands/moffs.h"
#include "src/operands/r.h"
#include "src/operands/rel.h"
#include "src/operands/sreg.h"
#include "src/operands/st.h"
#include "src/operands/xmm.h"
#include "src/operands/ymm.h"

namespace x64 {

class AttWriter {
	public:
		static void write(std::ostream& os, Cr c);
		static void write(std::ostream& os, Dr d);
		static void write(std::ostream& os, Eflag e);
		static void write(std::ostream& os, Imm i);
		static void write(std::ostream& os, Label l);
		static void write(std::ostream& os, Mm m);
		static void write(std::ostream& os, Moffs m);
		static void write(std::ostream& os, Rl r);
		static void write(std::ostream& os, Rh r);
		static void write(std::ostream& os, Rb r);
		static void write(std::ostream& os, R16 r);
		static void write(std::ostream& os, R32 r);
		static void write(std::ostream& os, R64 r);
		static void write(std::ostream& os, Rel r);
		static void write(std::ostream& os, Sreg s);
		static void write(std::ostream& os, St s);
		static void write(std::ostream& os, Xmm x);
		static void write(std::ostream& os, Ymm y);
};

} // namespace x64

#endif

