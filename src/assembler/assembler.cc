#include "src/assembler/assembler.h"

#include <cassert>
#include <iomanip>
#include <set>

using namespace std;
using namespace x64;

namespace {

typedef unsigned char byte;

inline void emit(byte c) {
	// TODO
}

inline void pref_group1(byte c) {
	emit(c);
}

inline void pref_group2(const M m) {
	static byte pref[6] {0x26, 0x2e, 0x36, 0x3e, 0x64, 0x65};
	if ( !m.null_seg() )
		emit(pref[m.get_seg().val_]);
}

inline void pref_group2(const Hint h) {
	emit(h == Hint::TAKEN ? 0x3e : 0x2e);
}

inline void pref_group3() {
	emit(0x66);
}

inline void pref_group4(const M m) {
	if ( m.get_addr_or() )
		emit(0x67);
}

inline void opcode(byte o1) {
	emit(o1);
}

inline void opcode(byte o1, Operand rcode) {
	const auto delta = rcode.val_ & 0x7;
	emit(o1 + delta);
}

inline void opcode(byte o1, byte o2) {
	emit(o1);
	emit(o2);
}

inline void opcode(byte o1, byte o2, byte o3) {
	emit(o1);
	emit(o2);
	emit(o3);
}

#if 0
inline void emit(unsigned char*& buf, unsigned char c) {
	(*buf++) = c;
}

inline void emit_mem_prefix(unsigned char*& buf, M m) {
	if ( m.get_addr_or() )
		emit(buf, 0x67);
}

inline void emit_prefix(unsigned char*& buf, unsigned char c) {
	emit(buf, c);
}

inline void emit_prefix(unsigned char*& buf, unsigned char c1,
		                    unsigned char c2) {
	emit(buf, c1);
	emit(buf, c2);
}

inline void emit_prefix(unsigned char*& buf, unsigned char c1,
		                    unsigned char c2, unsigned char c3) {
	emit(buf, c1);
	emit(buf, c2);
	emit(buf, c3);
}

inline void emit_opcode(unsigned char*& buf, unsigned char c) {
	emit(buf, c);
}

inline void emit_opcode(unsigned char*& buf, unsigned char c, Operand delta) {
	emit(buf, c + (0x7 & delta));
}

inline void emit_opcode(unsigned char*& buf, unsigned char c1, 
		                    unsigned char c2) {
	emit(buf, c1);
	emit(buf, c2);
}

inline void emit_opcode(unsigned char*& buf, unsigned char c1, 
		                    unsigned char c2, Operand delta) {
	emit(buf, c1);
	emit(buf, c2 + (0x7 & delta));
}

inline void emit_opcode(unsigned char*& buf, unsigned char c1, 
												unsigned char c2, unsigned char c3) {
	emit(buf, c1);
	emit(buf, c2);
	emit(buf, c3);
}

inline void emit_opcode(unsigned char*& buf, unsigned char c1, 
		                    unsigned char c2, unsigned char c3, Operand delta) {
	emit(buf, c1);
	emit(buf, c2);
	emit(buf, c3 + (0x7 & delta));
}

inline void emit_imm(unsigned char*& buf, Imm8 imm) {
	emit(buf, imm & 0xff); 
}

inline void emit_imm(unsigned char*& buf, Imm16 imm) {
	*((uint16_t*) buf) = imm;
	buf += 2;
}

inline void emit_imm(unsigned char*& buf, Imm32 imm) {
	*((uint32_t*) buf) = imm;
	buf += 4;
}

inline void emit_imm(unsigned char*& buf, Imm64 imm) {
	*((uint64_t*) buf) = imm;
	buf += 8;
}

// MOD R/M nop -- Simplifies codegen.
// This corresponds to instructions without any explicit operands.
// Calls to this method should be inlined away.
inline void emit_mod_rm(unsigned char*& buf) {
}

// MOD R/M nop -- Simplifies codegen.
// This corresponds to instructions with a single operand and no digit bit.
// This is the class of instructions that encode operand directly in opcode.
// See bswap for example.
// Calls to this method should be inlined away.
inline void emit_mod_rm(unsigned char*& buf, Operand ignore) {
}

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

// REX nop -- Simplifies codegen.
// Calls to this method should be inlined away.
inline void emit_rex(unsigned char*& buf, R low) {
}

// REX nop -- Simplifies codegen.
// Calls to this method should be inlined away.
inline void emit_rex(unsigned char*& buf, unsigned char rex, R low) {
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
#include "src/assembler/assembler.defn"

/*
void Assembler::start(Function& fxn) {
	start(fxn.buffer_);
}

void Assembler::assemble(const Instruction& i) {
	switch ( i.get_opcode() ) {
		case LABEL_DEFN:
			bind(i.get_operand(0));
			break;
        
   	// 4000-way switch
		//#include "src/gen/assembler.switch"
		
		default:
			assert(false);
			emit(buf_, 0x90);
	}
}

void Assembler::finish() {
	for ( const auto& jump : jumps_ ) {
		const auto pos = jump.first;
		const auto itr = labels_.find((Operand) jump.second);

		if ( itr == labels_.end() ) 
			*((uint32_t*) pos) = 0;
		else
			*((uint32_t*) pos) = itr->second-pos-4;
	}
}

void Assembler::write_binary(ostream& os, const Code& code) {
	static unsigned char buffer[1024*1024];
	start(buffer);

	for ( const auto& instr : code )
		assemble(instr);
	finish();

	for ( unsigned char* i = buf_begin_; i < buf_; ++i )
		os << *i;
}

void Assembler::write_hex(ostream& os, const Code& code) {
	static unsigned char buffer[1024*1024];
	start(buffer);

	set<unsigned char*> line_breaks;
	for ( const auto& instr : code ) {
		assemble(instr);
		line_breaks.insert(buf_);
	}
	finish();

	for ( unsigned char* i = buf_begin_; i < buf_; ++i ) {
		if ( line_breaks.find(i) != line_breaks.end() )
			os << endl;
		os << hex << setfill('0') << setw(2) << (int) *i << " ";
	}
}

void Assembler::start(unsigned char* buffer) {
	labels_.clear();
	jumps_.clear();

	buf_ = buf_begin_ = buffer;
}
*/

} // namespace x64
