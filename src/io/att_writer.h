#ifndef X64_SRC_IO_ATT_WRITER_H
#define X64_SRC_IO_ATT_WRITER_H

#include <iostream>

#include "src/code/code.h"
#include "src/code/cr.h"
#include "src/code/dr.h"
#include "src/code/eflag.h"
#include "src/code/imm.h"
#include "src/code/instruction.h"
#include "src/code/label.h"
#include "src/code/m.h"
#include "src/code/mm.h"
#include "src/code/moffs.h"
#include "src/code/opcode.h"
#include "src/code/op_set.h"
#include "src/code/r.h"
#include "src/code/rel.h"
#include "src/code/sreg.h"
#include "src/code/st.h"
#include "src/code/xmm.h"
#include "src/code/ymm.h"

namespace x64 {

class AttWriter {
	public:
		static void write(std::ostream& os, const Code& c);
		static void write(std::ostream& os, const Instruction& i);
		static void write(std::ostream& os, const Opcode o);

		static void write(std::ostream& os, const Cr0234 c);
		static void write(std::ostream& os, const Cr8 c);
		static void write(std::ostream& os, const Dr d);
		static void write(std::ostream& os, const Eflag e);
		static void write(std::ostream& os, const Imm8 i);
		static void write(std::ostream& os, const Imm16 i);
		static void write(std::ostream& os, const Imm32 i);
		static void write(std::ostream& os, const Imm64 i);
		static void write(std::ostream& os, const Zero z);
		static void write(std::ostream& os, const One o);
		static void write(std::ostream& os, const Three t);
		static void write(std::ostream& os, const Label l);
		static void write(std::ostream& os, const M& m);
		static void write(std::ostream& os, const Mm m);
		static void write(std::ostream& os, const Moffs8 m);
		static void write(std::ostream& os, const Moffs16 m);
		static void write(std::ostream& os, const Moffs32 m);
		static void write(std::ostream& os, const Moffs64 m);
		static void write(std::ostream& os, const Rl r);
		static void write(std::ostream& os, const Rh r);
		static void write(std::ostream& os, const Rb r);
		static void write(std::ostream& os, const R16 r);
		static void write(std::ostream& os, const R32 r);
		static void write(std::ostream& os, const R64 r);
		static void write(std::ostream& os, const Rel8 r);
		static void write(std::ostream& os, const Rel32 r);
		static void write(std::ostream& os, const Sreg s);
		static void write(std::ostream& os, const St s);
		static void write(std::ostream& os, const Xmm x);
		static void write(std::ostream& os, const Ymm y);

		static void write(std::ostream& os, const OpSet& o);
};

} // namespace x64

#endif

