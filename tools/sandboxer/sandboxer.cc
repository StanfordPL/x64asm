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

	assm_.pushq(rbp);
	assm_.pushq(rbx);
	assm_.pushq(r12);
	assm_.pushq(r13);
	assm_.pushq(r14);
	assm_.pushq(r15);

	for ( const auto& instr : code ) {
		if ( instr.is_jump() )
			sandbox_jump(s);
		else if ( instr.is_ret() ) {
			assm_.popq(r15);
			assm_.popq(r14);
			assm_.popq(r13);
			assm_.popq(r12);
			assm_.popq(rbx);
			assm_.popq(rbp);
		}
		assm_.assemble(instr);	
	}
	
	sandbox_runaway(s);
	sandbox_jump_final(s);

	assm_.finish();

	return fxn;
}

void Sandboxer::sandbox_runaway(Sandbox& s) {
	// Okay to clobber rax at this point
	assm_.movq(rax, Imm64(0x1));
	assm_.movabsq(Moffs64(&s.runaway_), rax);
	assm_.popq(r15);
	assm_.popq(r14);
	assm_.popq(r13);
	assm_.popq(r12);
	assm_.popq(rbx);
	assm_.popq(rbp);
	assm_.retq();
}

void Sandboxer::sandbox_jump(Sandbox& s) {
	// Backup original rax
	assm_.pushq(rax); 
	// Backup condition regiters
	assm_.lahf();         
	assm_.pushw(ax); 

	// Decrement counter and exit abnormally on zero
	assm_.movabsq(rax, Moffs64(&s.max_jumps_));
	assm_.decq(rax);
	assm_.je(max_jump_exit_);

	// Normal writebase/fixup code 
	assm_.movabsq(Moffs64(&s.max_jumps_), rax);
	assm_.popw(ax);
	assm_.sahf();
	assm_.popq(rax);
}

void Sandboxer::sandbox_jump_final(Sandbox& s) {
	assm_.bind(max_jump_exit_);

	// If control is here, it missed this writeback/fixup code
	assm_.movabsq(Moffs64(&s.max_jumps_), rax);
	assm_.popw(ax);
	assm_.sahf();
	assm_.popq(rax);

	assm_.popq(r15);
	assm_.popq(r14);
	assm_.popq(r13);
	assm_.popq(r12);
	assm_.popq(rbx);
	assm_.popq(rbp);

	assm_.retq();
}

}
