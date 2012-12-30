#ifndef X64_SRC_IO_ATT_WRITER_H
#define X64_SRC_IO_ATT_WRITER_H

#include <iostream>

#include "src/code/code.h"
#include "src/code/instruction.h"
#include "src/code/opcode.h"
#include "src/operands/cr.h"
#include "src/operands/dr.h"
#include "src/operands/eflag.h"
#include "src/operands/imm.h"
#include "src/operands/label.h"
#include "src/operands/m.h"
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
		static void write(std::ostream& os, const Code& c);
		static void write(std::ostream& os, const Instruction& i);
		static void write(std::ostream& os, const Opcode o);

		static void write(std::ostream& os, const Cr c);
		static void write(std::ostream& os, const Dr d);
		static void write(std::ostream& os, const Eflag e);
		static void write(std::ostream& os, const Imm i);
		static void write(std::ostream& os, const Label l);
		static void write(std::ostream& os, const M m);
		static void write(std::ostream& os, const Mm m);
		static void write(std::ostream& os, const Moffs m);
		static void write(std::ostream& os, const NoRexR8 r);
		static void write(std::ostream& os, const RexR8 r);
		static void write(std::ostream& os, const Rl r);
		static void write(std::ostream& os, const Rh r);
		static void write(std::ostream& os, const Rb r);
		static void write(std::ostream& os, const R16 r);
		static void write(std::ostream& os, const R32 r);
		static void write(std::ostream& os, const R64 r);
		static void write(std::ostream& os, const Rel r);
		static void write(std::ostream& os, const Sreg s);
		static void write(std::ostream& os, const St s);
		static void write(std::ostream& os, const Xmm x);
		static void write(std::ostream& os, const Ymm y);
};

} // namespace x64

#endif

