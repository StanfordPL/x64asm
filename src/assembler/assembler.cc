#include "src/assembler/assembler.h"

#include <cassert>
#include <iomanip>
#include <set>

#include "src/att/att_writer.h"

using namespace std;
using namespace x64;

namespace {

inline void emit(unsigned char*& buf, unsigned char c) {
	(*buf++) = c;
}

inline void emit_mem_prefix(unsigned char*& buf, M m) {
	if ( m.get_size_or() )
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

	const auto mod = 0xc0 | ((r & 0x7) << 3) | (rm & 0x7);
	emit(buf, mod);
}

// This ignores the distinction between high and low general purpose regs,
//   It won't work correctly for AH, BH, CH, DH
inline void emit_mod_rm(unsigned char*& buf, M rm, Operand r) {
	assert(!((R64)r).is_null());
	assert(!((M)rm).is_null());
	assert(!rm.get_scale().is_null());

	auto idx = rm.get_index();

	// base is stored in [44:40]
	const auto base  = (rm >> 40) & 0x7;
	const auto no_index = idx.is_null();
	const auto index = no_index ? 0x4 : idx & 0x7;

	const auto disp  = (int32_t) rm.get_disp();
	const auto disp0 = disp == 0 && base != 0x5;
	const auto disp8 = disp < 128 && disp >= -128;

	auto mod = (disp0 ? 0x00 : disp8 ? 0x40 : 0x80) |
	           ((r << 3) & 0x38);

	// ModR/M and no SIB byte
	if ( base != 0x4 && no_index )
		emit(buf, mod | base);
	// ModR/M and SIB byte
	else {
		emit(buf, mod | 0x4);

		// Scale is stored in [34:32] 
		const auto sib = ((rm >> (32 - 6)) & 0xc0) |
		                 (index << 3) |
										 base;
		emit(buf, sib);
	}

	// Emit displacement
	if ( disp == 0 ) {
		if ( base == 0x5 )
			emit_imm(buf, Imm8(0));
	}
	else if ( disp8 )
		emit_imm(buf, (Imm8)disp);
	else
		emit_imm(buf, (Imm32)disp);
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

} // namespace

namespace x64 {

void Assembler::start(Function& fxn) {
	start(fxn.buffer_);
}

void Assembler::assemble(const Instruction& i) {
	switch ( i.get_opcode() >> 50 ) {
		case (LABEL_DEFN_64L >> 50):
			bind(i.get_operand(0));
			break;
        
      	// 4000-way switch
		#include "src/gen/assembler.switch"
		
		default:
			assert(false);
			emit(buf_, 0x90);
	}
}

void Assembler::finish() {
	for ( const auto& jump : jumps_ ) {
		auto pos = jump.first;
		const auto itr = labels_.find(jump.second);
		
		if ( itr == labels_.end() ) 
			emit_imm(pos, Imm32(0));
		else
			emit_imm(pos, (Imm32)(itr->second-pos-4));
	}
}

// void Assembler::adcb(Imm arg0) { } ...
#include "src/gen/assembler.defn"

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

} // namespace x64
