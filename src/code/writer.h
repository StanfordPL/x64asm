#ifndef X64_SRC_CODE_WRITER_H
#define X64_SRC_CODE_WRITER_H

#include <iostream>

#include "src/code/code.h"
#include "src/code/imm.h"
#include "src/code/label.h"
#include "src/code/m.h"
#include "src/code/mm.h"
#include "src/code/moffs.h"
#include "src/code/opcode.h"
#include "src/code/operand.h"
#include "src/code/r.h"
#include "src/code/scale.h"
#include "src/code/sreg.h"
#include "src/code/st.h"
#include "src/code/xmm.h"

namespace x64 {

class Writer {
	public:
		void write_att(std::ostream& os, M m) const;
		void write_att(std::ostream& os, const Code& code) const;
		void write_att(std::ostream& os, St st) const;
		void write_att(std::ostream& os, R r, BitWidth w) const;
		void write_att(std::ostream& os, Imm i, BitWidth w) const;
		void write_att(std::ostream& os, const Instruction& instr) const;
		void write_att(std::ostream& os, Label l) const;
		void write_att(std::ostream& os, Mm m) const;
		void write_att(std::ostream& os, Moffs o) const;
		void write_att(std::ostream& os, Opcode o) const;
		void write_att(std::ostream& os, Scale s) const;
		void write_att(std::ostream& os, Sreg s) const;
		void write_att(std::ostream& os, Xmm x) const;
};

} // namespace x64

#endif

