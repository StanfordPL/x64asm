#include "src/tracer/tracer.h"

#include "src/tracer/state.h"

using namespace x64;

// Some general notes:
// This code uses r14 and r15 as temporary registers.
// r15: used to store a pointer to the current trace state.
// r14: used to store memory addresses between before and after traces.
//      also used as a temp register when not otherwise engaged.

namespace {

// Backup registers used to generate the trace
inline void push(Assembler& assm) {
	assm.pushq(r15);
	assm.pushq(r14);
	assm.pushfq();
}

// Restore registers used to generate the trace
inline void pop(Assembler& assm) {
	assm.popfq();
	assm.popq(r14);
	assm.popq(r15);
}

// Clear the trace (sets next_elem_ to zero)
inline void clear(Assembler& assm, void* next_elem) {
	assm.pushq(r14);
	assm.pushfq();
	assm.movq(r14, Imm64(next_elem));
	assm.movq(M64(r14), Imm32(0));
	assm.popfq();
	assm.popq(r14);	
}

// Increment the next_elem_ pointer
inline void inc_next_elem(Assembler& assm, void* next_elem) {
	assm.pushq(r14);
	assm.pushfq();
	assm.movq(r14, Imm64(next_elem));
	assm.incq(M64(r14));
	assm.popfq();
	assm.popq(r14);
}

// Stores &trace_[next_elem_-1] in r15
// r15 = &t.trace_[next_elem_];
inline void get_next_elem(Assembler& assm, void* trace, void* next_elem) {
	assm.pushq(r14);
	assm.movq(r14, Imm64(next_elem));
	assm.imulq(r14, M64(r14), Imm32(sizeof(State)));
	assm.subq(r14, Imm32(sizeof(State)));
	assm.movq(r15, Imm64(trace));
	assm.addq(r15, r14);
	assm.popq(r14);
}

} // namespace

namespace x64 {

#define offset(type, mem) ((size_t)(&((type*)0)->mem))

Trace& Tracer::trace(Trace& t, const Code& code) {
	assm_.start(t.fxn_);

	clear(assm_, &t.next_elem_);
	for ( size_t i = 0, ie = code.size(); i < ie; ++i ) {
		const auto& instr = code[i];

		if ( instr.is_label_defn() ) {
			assm_.assemble(instr);
			continue;
		}

		inc_next_elem(assm_, &t.next_elem_);
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

	// Record memory (yikes!)
	if ( instr.touches_mem() && instr.mem_modifier() != NONE ) {
		const auto mi = instr.mem_index();
		const auto mem_addr_disp = Imm32(offset(State, addr_));
		const auto mem_disp = is_before ?
			Imm32(offset(State, mem_before_)) : Imm32(offset(State, mem_after_));

		// If this is a before trace, first record the effective address
		// Careful! RSP needs fixing first!
		if ( is_before ) {
			assm_.addq(rsp, Imm32(32));
			assm_.leaq(r14, (M8)instr.get_operand(mi));
			assm_.subq(rsp, Imm32(32));
		}

		// NOW it's safe to recompute r15
		get_next_elem(assm_, &t.trace_[0], &t.next_elem_);

		// Record the address and size in a before trace...
		if ( is_before ) {
			assm_.movq(M64(r15, mem_addr_disp), r14);

			const auto mem_size_disp = Imm32(offset(State, size_));
			switch ( instr.type(mi) ) {
				case M_8:   assm_.movq(M64(r15, mem_size_disp), Imm32(1));  break;
				case M_16:  assm_.movq(M64(r15, mem_size_disp), Imm32(2));  break;
				case M_32:  assm_.movq(M64(r15, mem_size_disp), Imm32(4));  break;
				case M_64:  assm_.movq(M64(r15, mem_size_disp), Imm32(8));  break;
				case M_80:  assert(false);                                  break;
				case M_128: assm_.movq(M64(r15, mem_size_disp), Imm32(16)); break;
				case M_256: assert(false);                                  break;
				default:    assert(false);
			}
		}
		// ... Reload it in an after trace
		else
			assm_.movq(r14, M64(r15, mem_addr_disp));

		// Either way, now we can record the value
		switch ( instr.type(mi) ) {
			case M_8:
				assm_.movb(r14b, M8(r14));
				assm_.movb(M8(r15, mem_disp), r14b);
				break;
			case M_16:
				assm_.movw(r14w, M16(r14));
				assm_.movw(M16(r15, mem_disp), r14w);
				break;
			case M_32:
				assm_.movl(r14d, M32(r14));
				assm_.movl(M32(r15, mem_disp), r14d);
				break;
			case M_64:
				assm_.movq(r14, M64(r14));
				assm_.movq(M64(r15, mem_disp), r14);
				break;
			case M_80:
				assert(false);
				break;
			case M_128:
				assm_.movaps(M128(rsp, Imm32(-16)), xmm0);
				assm_.movaps(xmm0, M128(r14));
				assm_.movaps(M128(r15, mem_disp), xmm0);
				assm_.movaps(xmm0, M128(rsp, Imm32(-16)));
				break;
			case M_256:
				assert(false);
				break;

			default:
				assert(false);
		}
	}
	// For anything other than memory, just go ahead and compute r15
	else
		get_next_elem(assm_, &t.trace_[0], &t.next_elem_);

	// Write out general purpose (we'll have to special case r14 - r15)
	// Careful! RSP needs fixing first.
	assm_.addq(rsp, Imm32(32));
	for ( auto r = R64::begin(), re = R64::end()-2; r != re; ++r ) {
		const auto r_disp = is_before ? 
			Imm32(offset(State, r_before_) + *r * sizeof(State::r_val_type)) :
			Imm32(offset(State, r_after_)  + *r * sizeof(State::r_val_type));
		assm_.movq(M64(r15, r_disp), *r);
	}
	assm_.subq(rsp, Imm32(32));
	// TODO - Special case for r14 - r15

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
	assm_.movq(r14, M64(rsp));
	assm_.movq(M64(r15, cond_disp), r14);

	pop(assm_);
}

} // namespace x64
