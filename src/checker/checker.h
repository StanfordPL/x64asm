#ifndef X64_SRC_CHECKER_CHECKER_H
#define X64_SRC_CHECKER_CHECKER_H

#include "src/code/code.h"
#include "src/code/cr.h"
#include "src/code/dr.h"
#include "src/code/eflag.h"
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

struct Checker {
	static bool check(Cr c);
	static bool check(Cr0234 c);
	static bool check(Cr8 c);

	static bool check(Dr d);

	static bool check(Eflag e);

	static bool check(Imm i);
	static bool check(Imm8 i);
	static bool check(Imm16 i);
	static bool check(Imm32 i);
	static bool check(Imm64 i);
	static bool check(Zero z);
	static bool check(One o);
	static bool check(Three t);

	static bool check(Instruction i);

	static bool check(Label l);

	static bool check(M m);

	static bool check(Mm m);

	static bool check(Pref66 p);
	static bool check(PrefRexW p);
	static bool check(Far f);

	static bool check(Moffs m);

	static bool check(NoRex8 r);
	static bool check(Rex8 r);
	static bool check(Rh r);
	static bool check(Rl r);
	static bool check(Rb r);
	static bool check(Al r);
	static bool check(Cl r);
	static bool check(R16 r);
	static bool check(Ax r);
	static bool check(Dx r);
	static bool check(R32 r);
	static bool check(Eax r);
	static bool check(R64 r);
	static bool check(Rax r);

	static bool check(Rel r);
	static bool check(Rel8 r);
	static bool check(Rel32 r);

	static bool 
};

} // namespace x64

#endif
