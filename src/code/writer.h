#ifndef X64_SRC_CODE_WRITER_H
#define X64_SRC_CODE_WRITER_H

#include <iostream>

#include "src/code/addr.h"
#include "src/code/code.h"
#include "src/code/fp_reg.h"
#include "src/code/gp_reg.h"
#include "src/code/imm.h"
#include "src/code/label.h"
#include "src/code/mmx_reg.h"
#include "src/code/offset.h"
#include "src/code/opcode.h"
#include "src/code/operand.h"
#include "src/code/scale.h"
#include "src/code/seg_reg.h"
#include "src/code/xmm_reg.h"

namespace x64 {

class Writer {
	public:
		void write_att(std::ostream& os, Addr addr) const;
		void write_att(std::ostream& os, const Code& code) const;
		void write_att(std::ostream& os, FpReg fp) const;
		void write_att(std::ostream& os, GpReg gp, BitWidth w) const;
		void write_att(std::ostream& os, Imm i, BitWidth w) const;
		void write_att(std::ostream& os, const Instruction& instr) const;
		void write_att(std::ostream& os, Label l) const;
		void write_att(std::ostream& os, MmxReg mm) const;
		void write_att(std::ostream& os, Offset o) const;
		void write_att(std::ostream& os, Opcode o) const;
		void write_att(std::ostream& os, Scale s) const;
		void write_att(std::ostream& os, SegReg seg) const;
		void write_att(std::ostream& os, XmmReg xmm) const;
};

} // namespace x64

#endif

