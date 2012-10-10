#include "src/sandboxer/sandboxer.h"

#include "src/sandboxer/sandbox.h"

namespace x64 {

Function& Sandboxer::sandbox(Function& fxn, Sandbox& s, const Code& code) {
	assm_.start(fxn);

	for ( const auto& instr : code )
		assm_.assemble(instr);	
	
	sandbox_runaway(s);

	return fxn;
}

void Sandboxer::sandbox_runaway(Sandbox& s) {
	// Okay to clobber rax at this point
	assm_.movq_64r_64i(rax, (Operand) 0x1);
	assm_.movabsq_64o_64rax((Operand) &s.runaway_, rax);
	assm_.retq();
}

}
