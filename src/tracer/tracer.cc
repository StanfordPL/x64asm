#include "src/tracer/tracer.h"

#include "src/tracer/state.h"

#include <iostream>
using namespace std;

using namespace x64;

namespace {

// Backup registers used to generate the trace
inline void push(Assembler& assm) {
	assm.pushq(r15);
	assm.pushq(r14);
	assm.pushq(r13);
	assm.pushfq();
}

// Restore registers used to generate the trace
inline void pop(Assembler& assm) {
	assm.popfq();
	assm.popq(r13);
	assm.popq(r14);
	assm.popq(r15);
}

// Recompute registers used to generate the trace
// r13 holds temp values
// r14 = &t.next_elem_;
// r15 = &t.trace_[next_elem_];
inline void recompute(Assembler& assm, void* next_elem, void* trace,
		                  bool is_before) {
	assm.movq(r14, Imm64(next_elem));
	assm.imulq(r15, M64(r14), Imm32(sizeof(State)));
	if ( !is_before )
		assm.subq(r15, Imm32(sizeof(State)));
	assm.movq(r14, Imm64(trace));
	assm.addq(r15, r14);
	assm.movq(r14, Imm64(next_elem));
}

} // namespace

namespace x64 {

#define offset(type, mem) ((size_t)(&((type*)0)->mem))

Trace& Tracer::trace(Trace& t, const Code& code) {
	assm_.start(t.fxn_);
	for ( size_t i = 0, ie = code.size(); i < ie; ++i ) {
		const auto& instr = code[i];
		
		trace(t, instr, i, true);
		assm_.assemble(instr);
		if ( !instr.is_ret() && !instr.is_jump() )
			trace(t, instr, i, false);
	}
	assm_.finish();

	return t;
}

void Tracer::trace(Trace& t, const Instruction& instr, size_t line,
		               bool is_before) {
	push(assm_);

	// Record memory
	// Careful! This is like executing code!
	// If we do this, everything (including rsp!) has to look untouched.
	// Figure out the effective address BEFORE we compute the temp registers.
	if ( instr.touches_mem() && instr.mem_modifier() != NONE ) {
		assm_.addq(rsp, Imm32(32));
		const auto mi = instr.mem_index();
		const auto mem_addr_disp = Imm32(offset(State, addr_));
		const auto mem_size_disp = Imm32(offset(State, size_));
		const auto mem_disp = is_before ?
			Imm32(offset(State, mem_before_)) : Imm32(offset(State, mem_after_));

		// Calculate the effective address
		assm_.leaq(r13, (M8)instr.get_operand(mi));

		// NOW recompute all our temp registers and store the trace
		recompute(assm_, &t.next_elem_, &t.trace_[0], is_before);
		assm_.movq(M64(r15, mem_addr_disp), r13);

		// Size and value are type-specific
		switch ( instr.type(mi) ) {
			case M_8:
				assm_.movb(r13b, (M8)instr.get_operand(mi));
				assm_.movb(M8(r15, mem_disp), r13b);
				assm_.movq(M64(r15, mem_size_disp), Imm32(1));
				break;
			case M_16:
				assm_.movw(r13w, (M16)instr.get_operand(mi));
				assm_.movw(M16(r15, mem_disp), r13w);
				assm_.movq(M64(r15, mem_size_disp), Imm32(2));
				break;
			case M_32:
				assm_.movl(r13d, (M32)instr.get_operand(mi));
				assm_.movl(M32(r15, mem_disp), r13d);
				assm_.movq(M64(r15, mem_size_disp), Imm32(4));
				break;
			case M_64:
				assm_.movq(r13, (M64)instr.get_operand(mi));
				assm_.movq(M64(r15, mem_disp), r13);
				assm_.movq(M64(r15, mem_size_disp), Imm32(8));
				break;
			case M_80:
				assert(false);
				break;
			case M_128:
				assm_.movaps(M128(rsp, Imm32(-16)), xmm0);
				assm_.movaps(xmm0, (M128)instr.get_operand(mi));
				assm_.movaps(M128(r15, mem_disp), xmm0);
				assm_.movaps(xmm0, M128(rsp, Imm32(-16)));
				assm_.movq(M64(r15, mem_size_disp), Imm32(16));
				break;
			case M_256:
				assert(false);
				break;

			default:
				assert(false);
		}
		assm_.subq(rsp, Imm32(32));
	}
	// Business as usual if we don't have to worry about memory.
	else 
		recompute(assm_, &t.next_elem_, &t.trace_[0], is_before);

	// Write out general purpose (we'll have to special case r13 - r15)
	// Careful!  rsp isn't correct here
	assm_.addq(rsp, Imm32(32));
	for ( auto r = R64::begin(), re = R64::end()-3; r != re; ++r ) {
		const auto r_disp = is_before ? 
			Imm32(offset(State, r_before_) + *r * sizeof(State::r_val_type)) :
			Imm32(offset(State, r_after_)  + *r * sizeof(State::r_val_type));
		assm_.movq(M64(r15, r_disp), *r);
	}
	assm_.subq(rsp, Imm32(32));
	// TODO - Special case for r13 - r15

	// Write out XMM Registers
	for ( auto xmm = Xmm::begin(), xmme = Xmm::end(); xmm != xmme; ++xmm ) {
		const auto x_disp = is_before ? 
			Imm32(offset(State, xmm_before_) + *xmm * sizeof(State::xmm_val_type)) :
			Imm32(offset(State, xmm_after_)  + *xmm * sizeof(State::xmm_val_type));
		assm_.movdqa(M128(r15, x_disp), *xmm);
	}

	// Write out the condition registers (they're on top of the stack)
	const auto cond_disp = is_before ?
		Imm32(offset(State, cond_before_)) : Imm32(offset(State, cond_after_));
	assm_.movq(r13, M64(rsp));
	assm_.movq(M64(r15, cond_disp), r13);

	// If this is a before trace, record line number and increment next_elem_
	if ( is_before ) {
		assm_.movq(M64(r15), Imm32(line));
		assm_.incq(M64(r14));
	}	

	pop(assm_);
}

} // namespace x64
