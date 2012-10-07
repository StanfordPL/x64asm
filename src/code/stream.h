#ifndef X64_SRC_CODE_STREAM_H
#define X64_SRC_CODE_STREAM_H

#include <iostream>

#include "src/code/addr.h"
#include "src/code/code.h"
#include "src/code/cond_reg.h"
#include "src/code/gp_reg.h"
#include "src/code/imm.h"
#include "src/code/instruction.h"
#include "src/code/label.h"
#include "src/code/opcode.h"
#include "src/code/operand.h"
#include "src/code/scale.h"
#include "src/code/seg_reg.h"
#include "src/code/xmm_reg.h"

namespace x64 {

/** iostream input/output formats.
*/
enum FormatVal {
	ATT = 0,

	FORMAT_VAL_NULL,
	NUM_FORMAT_VALS = FORMAT_VAL_NULL
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

/** iostream formatting manipulator.
*/
class width {
	public:
		inline explicit width(BitWidth w) 
				: w_(w) {
		}

		inline operator BitWidth() const {
			return w_;
		}

	private:
		BitWidth w_;
};

} // namespace x64

std::istream& operator>>(std::istream& is, const x64::format& f);
std::ostream& operator<<(std::ostream& os, const x64::format& f);

std::ostream& operator<<(std::ostream& os, const x64::width& w);

std::istream& operator>>(std::istream& is, x64::Addr& a);
std::istream& operator>>(std::istream& is, x64::Code& c);
std::istream& operator>>(std::istream& is, x64::CondReg& c);
std::istream& operator>>(std::istream& is, x64::GpReg& g);
std::istream& operator>>(std::istream& is, x64::Imm& i);
std::istream& operator>>(std::istream& is, x64::Instruction& i);
std::istream& operator>>(std::istream& is, x64::Label& l); 
std::istream& operator>>(std::istream& is, x64::Opcode& o); 
std::istream& operator>>(std::istream& is, x64::Scale& s); 
std::istream& operator>>(std::istream& is, x64::SegReg& s); 
std::istream& operator>>(std::istream& is, x64::XmmReg& x);

std::ostream& operator<<(std::ostream& os, const x64::Addr& a);
std::ostream& operator<<(std::ostream& os, const x64::Code& c);
std::ostream& operator<<(std::ostream& os, const x64::CondReg& c);
std::ostream& operator<<(std::ostream& os, const x64::GpReg& g);
std::ostream& operator<<(std::ostream& os, const x64::Imm& i);
std::ostream& operator<<(std::ostream& os, const x64::Instruction& i);
std::ostream& operator<<(std::ostream& os, const x64::Label& l); 
std::ostream& operator<<(std::ostream& os, const x64::Opcode& o); 
std::ostream& operator<<(std::ostream& os, const x64::Scale& s);
std::ostream& operator<<(std::ostream& os, const x64::SegReg& s);
std::ostream& operator<<(std::ostream& os, const x64::XmmReg& x);

#endif
