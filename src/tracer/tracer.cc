#include "src/tracer/tracer.h"

#include "src/assembler/assembler.h"
#include "src/tracer/state.h"
#include "src/tracer/trace.h"

#include <iostream>
using namespace std;

#define offset(type, mem) ((size_t)(&((type*)0)->mem))

namespace x64 {

Function& Tracer::trace(Function& fxn, Trace& t, const Code& code) {
	assm_.start(fxn);

	for ( size_t i = 0, ie = code.size(); i < ie; ++i ) {
		const auto& instr = code[i];
		
		auto before = befores_.find(i) != befores_.end();
		auto after = afters_.find(i) != afters_.end();

		if ( before )
			trace<true>(t);
		if ( before || after )
			finish_state(t, i);
		assm_.assemble(instr);
		if ( after )
			trace<false>(t);
	}

	assm_.finish();

	return fxn;
}

template <bool is_before>
void Tracer::trace(Trace& t) {
	assm_.pushq(rax); 
	assm_.pushq(rbx); 
	assm_.pushfq();

	// Push whatever gp regs we care about onto the stack
	for ( auto r = r64_.begin(), re = r64_.end(); r != re; ++r )
		assm_.pushq(*r);

	// Find the address of the current State
	// If this is an after trace, you'll have to subtract 1
	assm_.movabsq(rax, Moffs64(&t.next_elem_));
	if ( !is_before )
		assm_.decq(rax);

	assm_.movq(rbx, Imm64(sizeof(State)));
	assm_.imulq(rbx, rax);
	assm_.movq(rax, Imm64(&t.trace_));
	assm_.addq(rbx, rax);
	
	// Pop the registers of the stack (reverse order!) and write to State
	for ( auto r = r64_.rbegin(), re = r64_.rend(); r != re; ++r ) {
		const auto disp = is_before ? 
			Imm32(offset(State, r64_before_) + *r * sizeof(State::r64_val_type)) :
			Imm32(offset(State, r64_after_)  + *r * sizeof(State::r64_val_type));
		assm_.popq(rax);
		assm_.movq(M64(rbx, disp), rax);
	}

	// Write out XMM Registers
	for ( auto xmm = xmm_.begin(), xmme = xmm_.end(); xmm != xmme; ++xmm ) {
		const auto disp = is_before ? 
			Imm32(offset(State, xmm_before_) + *xmm * sizeof(State::xmm_val_type)) :
			Imm32(offset(State, xmm_after_)  + *xmm * sizeof(State::xmm_val_type));
		assm_.movdqa(M128(rbx, disp), *xmm);
	}

	// Pop the condition registers off the stack 
	assm_.popfq();

	// Write them out to the state if we want them
	if ( conds_ ) {
		const auto disp = is_before ?
			Imm32(offset(State, cond_before_)) : Imm32(offset(State, cond_after_));
		assm_.lahf();
		assm_.movq(M64(rbx, disp), rax);
	}

	// Now clean everything up
	assm_.popq(rbx);
	assm_.popq(rax);
}

void Tracer::finish_state(Trace& t, size_t line) {
	assm_.pushq(rax);
	assm_.pushq(rbx);
	assm_.pushfq();

	// Record the line number of the current instruction
	assm_.movabsq(rax, Moffs64(&t.next_elem_));
	assm_.movq(rbx, Imm64(sizeof(State)));
	assm_.imulq(rbx, rax);
	assm_.movq(rax, Imm64(&t.trace_));
	assm_.movq(M64(rax, rbx), Imm32(line));

	// Increment the trace's next elem pointer
	assm_.movq(rax, Imm64(&t.next_elem_));
	assm_.incq(M64(rax));

	assm_.popfq();
	assm_.popq(rbx);
	assm_.popq(rax);
}

} // namespace x64
