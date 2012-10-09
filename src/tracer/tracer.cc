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


		//if ( before )
		//	trace_before(t.trace_[t.next_elem_], instr);
		assm_.assemble(instr);
		//if ( after )
		//	trace_after(t.trace_[t.next_elem_], instr);

		if ( before || after ) {
			assm_.pushq_64r(rax);
			assm_.pushq_64r(rbx);

			// t.trace_[t.next_elem_].line_ = i;
			assm_.movabsq_64rax_64o(rax, (Operand) &t.next_elem_);
			assm_.movq_64r_64i(rbx, (Operand) sizeof(State));
			assm_.imulq_64r_64r_rm1(rbx, rax);
			assm_.movq_64r_64i(rax, (Operand) &t.trace_);
			assm_.movq_64m_32i_rm0(Addr(rax, rbx), (Operand) i);

			// t.next_elem_++;
			assm_.movq_64r_64i(rax, (Operand) &t.next_elem_);
			assm_.incq_64m_rm0(Addr(rax));

			assm_.popq_64r(rbx);
			assm_.popq_64r(rax);
		}
	}

	assm_.finish();
	fxn_();

	return t;
}

void Tracer::trace_before(State& state, const Instruction& instr) {
	assm_.pushq_64r(rax);

	for ( const auto& gp : gp_regs_ ) {
		assm_.movq_64r_64r_rm0(rax, gp);
		assm_.movabsq_64o_64rax((Operand) &state.gp_before_[gp], rax);
	}

	assm_.popq_64r(rax);
}

void Tracer::trace_after(State& state, const Instruction& instr) {
	assm_.pushq_64r(rax);

	for ( const auto& gp : gp_regs_ ) {
		assm_.movq_64r_64r_rm0(rax, gp);
		assm_.movabsq_64o_64rax((Operand) &state.gp_after_[gp], rax);
	}

	assm_.popq_64r(rax);
}

} // namespace x64
