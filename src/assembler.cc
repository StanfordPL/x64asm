#include "src/assembler.h"

#include <iomanip>
#include <vector>

#include "src/constants.h"

using namespace std;

namespace x64 {

// void Assembler::adcb(Al arg0, Imm8 arg1) { } ...
#include "src/assembler.defn"

void Assembler::assemble(const Instruction& instr) {
	switch ( instr.get_opcode() ) {
		case LABEL_DEFN:
			bind(static_cast<const Label*>(instr.get_operand(0))->val());
			break;
   	// 4000-way switch
		#include "src/assembler.switch"
		
		default:
			assert(false);
	}
}

void Assembler::debug_att(std::ostream& os, const Code& c) {
	debug(os, c, true);
}

void Assembler::debug_intel(std::ostream& os, const Code& c) {
	debug(os, c, false);
}

void Assembler::write_elf(std::ostream& os, const Code& c) {
}

void Assembler::write_hex(std::ostream& os, const Code& c) {
	const auto fxn = assemble(c);
	for ( size_t i = 0, ie = fxn.size(); i < ie; ++i ) {
		os << hex << noshowbase << (int32_t)fxn.buffer_[i] << " ";
		if ( ((i%8) == 7) && ((i+1) != ie) )
			os << endl;
	}
}

void Assembler::mod_rm_sib(const M& rm, const AtomicOperand& r) {
	assert(rm.check());
	assert(r.check());

	// Every path we take needs these bits for the mod/rm byte
	const auto rrr = (r.val() << 3) & 0x38;

	// Is base null? 
	// This special case simplifies everything that follows.
	if ( !rm.contains_base() ) { 
		const auto mod_byte = 0x00 | rrr | 0x4;
		const auto sib_byte = ((int)rm.get_scale() & 0xc0) | 
			                    ((rm.get_index()->val() << 3) & 0x38) | 0x5;
		
		fxn_->emit_byte(mod_byte);
		fxn_->emit_byte(sib_byte);
		disp_imm(*rm.get_disp());

		return;
	}

	// Every path we take now requires the non-null base value.
	const auto bbb = rm.get_base()->val() & 0x7;

	// This logic determines what the value of the mod bits will be.
	// It also controls how many immediate bytes we emit later.
	const auto disp = (int32_t)rm.get_disp()->val();
	size_t mod = 0x40;
	if ( disp < -128 || disp >= 128 )
		mod = 0x80;
	else if ( disp == 0 && bbb != 0x5 )
		mod = 0x00;

	// Is index null?
	if ( !rm.contains_index() ) {
		const auto mod_byte = mod | rrr | 0x4;
		const auto sib_byte = ((int)rm.get_scale() & 0xc0) |
			                    ((rm.get_index()->val() << 3) & 0x38) | bbb;

		fxn_->emit_byte(mod_byte);
		fxn_->emit_byte(sib_byte);
	}
	// Is base sitting in the eip/rip+disp32 row?
	else if ( bbb == 0x4 ) {
		const auto mod_byte = mod | rrr | 0x4;
		const auto sib_byte = ((int)rm.get_scale() & 0xc0) | 0x20 | bbb;

		fxn_->emit_byte(mod_byte);
		fxn_->emit_byte(sib_byte);
	}
	// Easy times
	else {
		const auto mod_byte = mod | rrr | bbb;
		fxn_->emit_byte(mod_byte);
	}

	// This logic parallels the logic for the mod bit
	if ( mod == 0x40 )
		disp_imm(Imm8(disp));
	else if ( mod == 0x80 )
		disp_imm(Imm32(disp));
}

void Assembler::debug(ostream& os, const Code& c, bool att) {
	Function fxn;
	vector<size_t> eols;

	start(fxn);
	for ( const auto& instr : c ) {
		assemble(instr);
		eols.push_back(fxn.size());
	}
	finish();

	size_t idx = 0;
	for ( size_t i = 0, ie = c.size(); i < ie; ++i ) {
		if ( att )
			c[i].write_att(os);
		else
			c[i].write_intel(os);

		os << "( ";
		while ( idx != eols[i] )
			os << hex << noshowbase << (int32_t)fxn.buffer_[idx++] << " ";
		os << ")";

		if ( (i+1) != ie )
			os << endl;
	}
}

} // namespace x64
