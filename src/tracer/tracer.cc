#include "src/tracer/tracer.h"

#include "src/tracer/state.h"

namespace x64 {

#define offset(type, mem) ((size_t)(&((type*)0)->mem))

Trace& Tracer::trace(Trace& t, const Code& code) {
	assm_.start(t.fxn_);

	for ( size_t i = 0, ie = code.size(); i < ie; ++i ) {
		const auto& instr = code[i];
		
		// Backup temp registers that we'll use
		assm_.pushq(r15);
		assm_.pushq(r14);
		assm_.pushq(r13);
		assm_.pushfq();

		// r13 will hold temp values
		//  ... see below 
		// r14 = &t.next_elem_;
		assm_.movq(r14, Imm64(&t.next_elem_));
		// r15 = &t.trace_[next_elem_];
		assm_.movq(r15, Imm64(&t.trace_));
		assm_.imulq(r15, M64(r14), Imm32(sizeof(State)));

		trace(t, instr, i, true);
		assm_.assemble(instr);
		trace(t, instr, i, false);

		// Restore temp registers
		assm_.popfq();
		assm_.popq(r13);
		assm_.popq(r14);
		assm_.popq(r15);
	}

	assm_.finish();

	return t;
}

void Tracer::trace(Trace& t, const Instruction& instr, size_t line,
		               bool is_before) {
	// Write out general purpose (we'll have to special case r13 - r15)
	for ( auto r = R64::begin(), re = R64::end()-3; r != re; ++r ) {
		const auto r_disp = is_before ? 
			Imm32(offset(State, r_before_) + *r * sizeof(State::r_val_type)) :
			Imm32(offset(State, r_after_)  + *r * sizeof(State::r_val_type));
		assm_.movq(M64(r15, r_disp), *r);
	}
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

	// Record memory 
	if ( !instr.touches_mem() || instr.mem_modifier() == NONE )
		return;

	const auto mi = instr.mem_index();
	const auto mem_addr_disp = Imm32(offset(State, addr_));
	const auto mem_size_disp = Imm32(offset(State, size_));
	const auto mem_disp = is_before ?
		Imm32(offset(State, mem_before_)) : Imm32(offset(State, mem_after_));

	// Address	
	assm_.leaq(r13, (M8)instr.get_operand(mi));
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
}

} // namespace x64
