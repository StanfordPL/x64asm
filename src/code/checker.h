#ifndef X64_SRC_CODE_CHECKER_H
#define X64_SRC_CODE_CHECKER_H

#include "src/code/code.h"
#include "src/code/constants.h"
#include "src/code/cr.h"
#include "src/code/dr.h"
#include "src/code/eflag.h"
#include "src/code/hint.h"
#include "src/code/imm.h"
#include "src/code/instruction.h"
#include "src/code/label.h"
#include "src/code/m.h"
#include "src/code/mm.h"
#include "src/code/modifier.h"
#include "src/code/moffs.h"
#include "src/code/opcode.h"
#include "src/code/operand.h"
#include "src/code/r.h"
#include "src/code/rel.h"
#include "src/code/sreg.h"
#include "src/code/st.h"
#include "src/code/xmm.h"
#include "src/code/ymm.h"

namespace x64 {

class Checker {
	public:
		static bool check(const Instruction& i);

};

} // namespace x64

#endif
