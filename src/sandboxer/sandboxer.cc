#include "src/sandboxer/sandboxer.h"

#include "src/code/label.h"
#include "src/sandboxer/sandbox.h"

using namespace x64;

namespace {

const Label max_jump_exit_ = UINT_MAX;

}

namespace x64 {

Function& Sandboxer::sandbox(Function& fxn, Sandbox& s, const Code& code) {
	assm_.start(fxn);

	for ( const auto& instr : code ) {
		if ( instr.is_jump() )
			sandbox_jump(s);
		assm_.assemble(instr);	
	}
	
	sandbox_runaway(s);
	sandbox_jump_final(s);

	assm_.finish();

	return fxn;
}

void Sandboxer::sandbox_runaway(Sandbox& s) {
	// Okay to clobber rax at this point
	assm_.movq_64r_64i(rax, (Operand) 0x1);
	assm_.movabsq_64o_64rax((Operand) &s.runaway_, rax);
	assm_.retq();
}

void Sandboxer::sandbox_jump(Sandbox& s) {
	// Backup original rax
	assm_.pushq_64r(rax); 
	// Backup condition regiters
	assm_.lahf();         
	assm_.pushw_16r(rax); 

	// Decrement counter and exit abnormally on zero
	assm_.movabsq_64rax_64o(rax, (Operand) &s.max_jumps_);
	assm_.decq_64r_rm0(rax);
	assm_.je_64l(max_jump_exit_);

	// Normal writebase/fixup code 
	assm_.movabsq_64o_64rax((Operand) &s.max_jumps_, rax);
	assm_.popw_16r(rax);
	assm_.sahf();
	assm_.popq_64r(rax);
}

void Sandboxer::sandbox_jump_final(Sandbox& s) {
	assm_.bind(max_jump_exit_);

	// If control is here, it missed this writeback/fixup code
	assm_.movabsq_64o_64rax((Operand) &s.max_jumps_, rax);
	assm_.popw_16r(rax);
	assm_.sahf();
	assm_.popq_64r(rax);

	assm_.retq();
}

}
