#ifndef X64_SRC_ATT_ATT_WRITER_H
#define X64_SRC_ATT_ATT_WRITER_H

#include <iostream>

#include "src/code/code.h"
#include "src/code/imm.h"
#include "src/code/label.h"
#include "src/code/m.h"
#include "src/code/mm.h"
#include "src/code/moffs.h"
#include "src/code/opcode.h"
#include "src/code/r.h"
#include "src/code/scale.h"
#include "src/code/sreg.h"
#include "src/code/st.h"
#include "src/code/xmm.h"
#include "src/code/ymm.h"

namespace x64 {

class AttWriter {
	public:
		void write(std::ostream& os, const Code& code) const;
		void write(std::ostream& os, Imm8) const;
		void write(std::ostream& os, Imm16) const;
		void write(std::ostream& os, Imm32) const;
		void write(std::ostream& os, Imm64) const;
		void write(std::ostream& os, const Instruction& instr) const;
		void write(std::ostream& os, Label l) const;
		void write(std::ostream& os, M m) const;
		void write(std::ostream& os, Mm m) const;
		void write(std::ostream& os, Moffs o) const;
		void write(std::ostream& os, Opcode o) const;
		void write(std::ostream& os, RH r) const;
		void write(std::ostream& os, R8 r) const;
		void write(std::ostream& os, R16 r) const;
		void write(std::ostream& os, R32 r) const;
		void write(std::ostream& os, R64 r) const;
		void write(std::ostream& os, Scale s) const;
		void write(std::ostream& os, Sreg s) const;
		void write(std::ostream& os, St st) const;
		void write(std::ostream& os, Xmm x) const;
		void write(std::ostream& os, Ymm y) const;
};

} // namespace x64

#endif

