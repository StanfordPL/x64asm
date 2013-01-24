#include "src/assembler.h"

#include <cassert>
#include <iomanip>
#include <set>
#include <vector>

using namespace std;
using namespace x64;

namespace {

#if 0
// This ignores the distinction between high and low general purpose regs,
//   It won't work correctly for AH, BH, CH, DH
inline void emit_mod_rm(unsigned char*& buf, Operand rm, Operand r) {
	assert(!((R64)rm).is_null());
	assert(!((R64)r).is_null());

	auto mod_byte = 0xc0 | ((r << 3) & 0x38) | (rm & 0x7);
	emit(buf, mod_byte);
}

// This ignores the distinction between high and low general purpose regs,
//   It won't work correctly for AH, BH, CH, DH
inline void emit_mod_rm(unsigned char*& buf, M rm, Operand r) {
	// Neither input should be null
	assert(!((R64)r).is_null());
	assert(!((M)rm).is_null());
	// Check that nullness is defined as being equal to 16
	assert(!rm.get_base().is_null() || rm.get_base() == 16);
	assert(!rm.get_index().is_null() || rm.get_index() == 16);
	// Check that both index and base aren't both null
	assert(!rm.get_base().is_null() || !rm.get_index().is_null());

	// Every path we take needs these bits for the mod/rm byte
	const auto rrr = (r << 3) & 0x38;

	// This special case simplifies all subsequent logic.  Is base null?
	// We don't need base, we can just check its null bit.
	if ( rm & (0x1ull << 44) ) {
		const auto mod_byte = 0x00 | rrr | 0x4;
		emit(buf, mod_byte);

		const auto sib_byte = ((rm >> (32-6)) & 0xc0) | ((rm >> (35-3)) & 0x38) | 0x5;
		emit(buf, sib_byte);

		// One bitmask faster than get_disp(): M and Imm32 agree on lowest 32 bits.
		emit_imm(buf, (Imm32) rm);
		return;
	}

	// At this point we don't have to worry about base being null anymore
	// And every path below here requires us to use the value of b, so let's
	// move it to bits 2-0 and be done with it in two instructions.
	const auto bbb = (rm >> 40) & 0x7;

	// This logic determines what the value of the mod bits will be.
	// It also controls how many immediate bytes we emit later.
	// Since one constant is as good as another, let's bit shift here.
	const auto disp = (int32_t) rm.get_disp();
	size_t mod = 0x40;
	if ( disp < -128 || disp >= 128 )
		mod = 0x80;
	else if ( disp == 0 && bbb != 0x5 )
		mod = 0x00;

	// Is the index byte's null bit set?
	if ( !(rm & (0x1ull << 39)) ) { 
		const auto mod_byte = mod | rrr | 0x4;
		emit(buf, mod_byte);

		const auto sib_byte = ((rm >> (32-6)) & 0xc0) | ((rm >> (35-3)) & 0x38) | bbb;
		emit(buf, sib_byte);
	}
	// Is base sitting in the eip/rip+disp32 row?
	else if ( bbb == 0x4 ) {
		const auto mod_byte = mod | rrr | 0x4;
		emit(buf, mod_byte);

		const auto sib_byte = ((rm >> (32-6)) & 0xc0) | 0x20 | bbb;
		emit(buf, sib_byte);
	}
	// Easy times
	else {
		const auto mod_byte = mod | rrr | bbb;
		emit(buf, mod_byte);
	}

	// This logic parallels the logic for the mod bit
	// I don't know if it's faster but we don't need disp for this to be correct.
	switch ( mod ) {
		case 0x40: emit_imm(buf, (Imm8) rm);  break;
		case 0x80: emit_imm(buf, (Imm32) rm); break;
		default: break;
	}
}

// Figure 2.7: Intel Manual Vol 2A 2-9
// This ignores the distinction between high and low general purpose regs,
//   but that's fine because it wouldn't get you an rex.b either way.
inline void emit_rex(unsigned char*& buf, Operand rm, unsigned char rex, 
		                 R low) {
	assert(!((R64)rm).is_null());
	rex |= (rm >> 3);

	if ( rex || (low >> 2 == 0x1) )
		emit(buf, rex | 0x40);
}

// Figure 2.5: Intel Manual Vol 2A 2-8
// This ignores the distinction between high and low general purpose regs,
//   but that's fine because it wouldn't get you an rex.b either way.
inline void emit_rex(unsigned char*& buf, Operand rm, Operand r, 
		                 unsigned char rex, R low) {
	assert(!((R64)rm).is_null());
	assert(!((R64)r).is_null());

	rex |= (rm >> 3);
	rex |= (r >> 1) & 0x4;

	if ( rex || (low >> 2 == 0x1) )
		emit(buf, rex | 0x40);
}

// Figure 2.4 & 2.6: Intel Manual Vol 2A 2-8 & 2.9
// This ignores the distinction between high and low general purpose regs,
//   but that's fine because it wouldn't get you an rex.b either way.
inline void emit_rex(unsigned char*& buf, M rm, Operand r, unsigned char rex, 
		                 R low) {
	assert(!((R64)r).is_null());

	rex |= (r >> 1) & 0x4;
	// base is stored in [44:40]
	rex |= (rm >> (40+3)) & 0x1;
	// index is stored in [39:35]
	rex |= (rm >> (35+2)) & 0x2;

	if ( rex || (low >> 2 == 0x1) )
		emit(buf, rex | 0x40);
}

// This is essentially identical to the case above.
// The only difference being that there is no possibility of setting rex.r.
inline void emit_rex(unsigned char*& buf, M rm, 
		                 unsigned char rex, R low) {
	// base is stored in [44:40]
	rex |= (rm >> (40+3)) & 0x1;
	// index is stored in [39:35]
	rex |= (rm >> (35+2)) & 0x2;

	// No check for is8bit since this only takes a mem argument
	assert(low.is_null());
	if ( rex )
		emit(buf, rex | 0x40);
}
#endif
} // namespace

namespace x64 {

// void Assembler::adcb(Imm arg0) { } ...
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

void Assembler::write_att(std::ostream& os, const Code& c) {
	write_txt(os, c, true);
}

void Assembler::write_intel(std::ostream& os, const Code& c) {
	write_txt(os, c, false);
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

void Assembler::write_txt(ostream& os, const Code& c, bool att) {
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
