#include "src/tracer/tracer.h"

#include "src/assembler/assembler.h"
#include "src/tracer/state.h"
#include "src/tracer/trace.h"

#include <iostream>
using namespace std; // DEBUG

namespace x64 {

#define offset(type, mem) ((size_t)(&((type*)0)->mem))

Trace& Tracer::trace(Trace& t, const Code& code) {
	assm_.start(fxn_);

	for ( size_t i = 0, ie = code.size(); i < ie; ++i ) {
		const auto& instr = code.get(i);
		
		auto before = befores_.find(i) != befores_.end();
		auto after = afters_.find(i) != afters_.end();

		// This is the easy case, if we're tracing before the instruction,
		//   just grab everything without thinking about it.
		if ( before ) {
			// Backup original rax and rbx
			assm_.pushq_64r(rax); 
			assm_.pushq_64r(rbx); 
			// Backup condition regiters (this clobbers rax, so get it back)
			assm_.lahf();         
			assm_.pushw_16r(rax); 
			assm_.movq_64r_64m_rm1(rax, Addr(rsp, Imm(10)));

			// Push everything we want to write out to the stack
			for ( auto gp = gp_regs_.begin(), gpe = gp_regs_.end(); gp != gpe; ++gp )
				assm_.pushq_64r(*gp);

			// Find the address of the current State
			assm_.movabsq_64rax_64o(rax, (Operand) &t.next_elem_);
			assm_.movq_64r_64i(rbx, (Operand) sizeof(State));
			assm_.imulq_64r_64r_rm1(rbx, rax);
			assm_.movq_64r_64i(rax, (Operand) &t.trace_);
			assm_.addq_64r_64r_rm0(rbx, rax);

			// Pop the gp registers of the stack (reverse order!) and write to State
			for ( auto gp = gp_regs_.rbegin(), gpe = gp_regs_.rend(); 
							gp != gpe; ++gp ) {
				assm_.popq_64r(rax);
				const auto disp = Imm(offset(State, gp_before_) + 
						                  *gp * sizeof(State::gp_reg_val_type));
				assm_.movq_64m_64r_rm0(Addr(rbx, disp), rax);
			}

			// Put things back like we found them
			assm_.popw_16r(rax);
			assm_.sahf();
			assm_.popq_64r(rbx);
			assm_.popq_64r(rax);
		}

		// If we're tracing either before or after this instruction,
		//   then we'll want to record its line number and increment next_elem_.
		if ( before || after ) {
			assm_.pushq_64r(rax);
			assm_.pushq_64r(rbx);
			assm_.lahf();
			assm_.pushw_16r(rax);

			// t.trace_[t.next_elem_].line_ = i;
			assm_.movabsq_64rax_64o(rax, (Operand) &t.next_elem_);
			assm_.movq_64r_64i(rbx, (Operand) sizeof(State));
			assm_.imulq_64r_64r_rm1(rbx, rax);
			assm_.movq_64r_64i(rax, (Operand) &t.trace_);
			assm_.movq_64m_32i_rm0(Addr(rax, rbx), (Operand) i);

			// t.next_elem_++;
			assm_.movq_64r_64i(rax, (Operand) &t.next_elem_);
			assm_.incq_64m_rm0(Addr(rax));

			assm_.popw_16r(rax);
			assm_.sahf();
			assm_.popq_64r(rbx);
			assm_.popq_64r(rax);
		}

		assm_.assemble(instr);
		//if ( after )
		//	trace_after(t.trace_[t.next_elem_], instr);

	}

	assm_.finish();
	fxn_();

	return t;
}

/*
void Tracer::trace_after(State& state, const Instruction& instr) {
	assm_.pushq_64r(rax);

	for ( const auto& gp : gp_regs_ ) {
		assm_.movq_64r_64r_rm0(rax, gp);
		assm_.movabsq_64o_64rax((Operand) &state.gp_after_[gp], rax);
	}

	assm_.popq_64r(rax);
}
*/

} // namespace x64
