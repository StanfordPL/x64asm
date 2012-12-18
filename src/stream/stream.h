#ifndef X64_SRC_STREAM_STREAM_H
#define X64_SRC_STREAM_STREAM_H

#include <iostream>

#include "src/code/cr.h"
#include "src/code/dr.h"
#include "src/code/eflag.h"
#include "src/code/imm.h"
#include "src/code/label.h"
#include "src/code/mm.h"
#include "src/code/moffs.h"
#include "src/code/r.h"
#include "src/code/rel.h"
#include "src/code/sreg.h"
#include "src/code/st.h"
#include "src/code/xmm.h"
#include "src/code/ymm.h"

std::ostream& operator<<(std::ostream& os, const x64::Cr c);
std::ostream& operator<<(std::ostream& os, const x64::Dr d);
std::ostream& operator<<(std::ostream& os, const x64::Eflag e);
std::ostream& operator<<(std::ostream& os, const x64::Imm i);
std::ostream& operator<<(std::ostream& os, const x64::Label l);
std::ostream& operator<<(std::ostream& os, const x64::Mm m);
std::ostream& operator<<(std::ostream& os, const x64::Moffs m);
std::ostream& operator<<(std::ostream& os, const x64::Rl r);
std::ostream& operator<<(std::ostream& os, const x64::Rh r);
std::ostream& operator<<(std::ostream& os, const x64::Rb r);
std::ostream& operator<<(std::ostream& os, const x64::R16 r);
std::ostream& operator<<(std::ostream& os, const x64::R32 r);
std::ostream& operator<<(std::ostream& os, const x64::R64 r);
std::ostream& operator<<(std::ostream& os, const x64::Rel r);
std::ostream& operator<<(std::ostream& os, const x64::Sreg s);
std::ostream& operator<<(std::ostream& os, const x64::St s);
std::ostream& operator<<(std::ostream& os, const x64::Xmm x);
std::ostream& operator<<(std::ostream& os, const x64::Ymm y);

#if 0
#include <iostream>

#include "src/code/code.h"
#include "src/code/cond_reg.h"
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

/** iostream input/output formats.
*/
enum FormatVal {
	ATT = 0,
	BIN,
	DOT,
	HEX
};

enum CodeFormatVal {
  ALL = 0,
  NOP_REMOVED,
  DEAD_REMOVED
};

/** iostream formatting manipulator.
*/
class format {
	public:
		inline explicit format(FormatVal f)
				: f_(f) {
		}
		inline operator FormatVal() const {
			return f_;
		}
	private:
		FormatVal f_;
};

class code_format {
  	public:
		inline explicit code_format(CodeFormatVal f)
				: f_(f) {
		}
		inline operator CodeFormatVal() const {
			return f_;
		}
	private:
		CodeFormatVal f_;

};

} // namespace x64

std::istream& operator>>(std::istream& is, const x64::format& f);
std::ostream& operator<<(std::ostream& os, const x64::format& f);

std::ostream& operator<<(std::ostream& os, const x64::code_format& f);

std::istream& operator>>(std::istream& is, x64::Code& c);

std::ostream& operator<<(std::ostream& os, const x64::Code& code);
std::ostream& operator<<(std::ostream& os, x64::CondReg cr);
std::ostream& operator<<(std::ostream& os, x64::Imm8 i);
std::ostream& operator<<(std::ostream& os, x64::Imm16 i);
std::ostream& operator<<(std::ostream& os, x64::Imm32 i);
std::ostream& operator<<(std::ostream& os, x64::Imm64 i);
std::ostream& operator<<(std::ostream& os, const x64::Instruction& instr);
std::ostream& operator<<(std::ostream& os, x64::Label l);
std::ostream& operator<<(std::ostream& os, x64::M m);
std::ostream& operator<<(std::ostream& os, x64::Mm m);
std::ostream& operator<<(std::ostream& os, x64::Moffs o);
std::ostream& operator<<(std::ostream& os, x64::Opcode o);
std::ostream& operator<<(std::ostream& os, x64::RH r);
std::ostream& operator<<(std::ostream& os, x64::R8 r);
std::ostream& operator<<(std::ostream& os, x64::R16 r);
std::ostream& operator<<(std::ostream& os, x64::R32 r);
std::ostream& operator<<(std::ostream& os, x64::R64 r);
std::ostream& operator<<(std::ostream& os, x64::Scale s);
std::ostream& operator<<(std::ostream& os, x64::Sreg s);
std::ostream& operator<<(std::ostream& os, x64::St st);
std::ostream& operator<<(std::ostream& os, x64::Xmm x);
std::ostream& operator<<(std::ostream& os, x64::Ymm y);
#endif

#endif
