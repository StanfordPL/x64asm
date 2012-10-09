#include "src/assembler/assembler.h"

#include <cassert>
#include <iomanip>
#include <set>

using namespace std;
using namespace x64;

namespace {

// Table 2.2 Intel Manual Vol 2A 2-5
unsigned char mod_rm_00_[8][8] 
	{{0x00,0x08,0x10,0x18,0x20,0x28,0x30,0x38},
   {0x01,0x09,0x11,0x19,0x21,0x29,0x31,0x39},
   {0x02,0x0a,0x12,0x1a,0x22,0x2a,0x32,0x3a},
   {0x03,0x0b,0x13,0x1b,0x23,0x2b,0x33,0x3b},
   {0x04,0x0c,0x14,0x1c,0x24,0x2c,0x34,0x3c},
   {0x05,0x0d,0x15,0x1d,0x25,0x2d,0x35,0x3d},
   {0x06,0x0e,0x16,0x1e,0x26,0x2e,0x36,0x3e},
   {0x07,0x0f,0x17,0x1f,0x27,0x2f,0x37,0x3f}};
unsigned char mod_rm_01_[8][8] 
	{{0x40,0x48,0x50,0x58,0x60,0x68,0x70,0x78},
   {0x41,0x49,0x51,0x59,0x61,0x69,0x71,0x79},
   {0x42,0x4a,0x52,0x5a,0x62,0x6a,0x72,0x7a},
   {0x43,0x4b,0x53,0x5b,0x63,0x6b,0x73,0x7b},
   {0x44,0x4c,0x54,0x5c,0x64,0x6c,0x74,0x7c},
   {0x45,0x4d,0x55,0x5d,0x65,0x6d,0x75,0x7d},
   {0x46,0x4e,0x56,0x5e,0x66,0x6e,0x76,0x7e},
   {0x47,0x4f,0x57,0x5f,0x67,0x6f,0x77,0x7f}};
unsigned char mod_rm_10_[8][8] 
	{{0x80,0x88,0x90,0x98,0xa0,0xa8,0xb0,0xb8},
   {0x81,0x89,0x91,0x99,0xa1,0xa9,0xb1,0xb9},
   {0x82,0x8a,0x92,0x9a,0xa2,0xaa,0xb2,0xba},
   {0x83,0x8b,0x93,0x9b,0xa3,0xab,0xb3,0xbb},
   {0x84,0x8c,0x94,0x9c,0xa4,0xac,0xb4,0xbc},
   {0x85,0x8d,0x95,0x9d,0xa5,0xad,0xb5,0xbd},
   {0x86,0x8e,0x96,0x9e,0xa6,0xae,0xb6,0xbe},
   {0x87,0x8f,0x97,0x9f,0xa7,0xaf,0xb7,0xbf}};
unsigned char mod_rm_11_[8][8] 
	{{0xc0,0xc8,0xd0,0xd8,0xe0,0xe8,0xf0,0xf8},
   {0xc1,0xc9,0xd1,0xd9,0xe1,0xe9,0xf1,0xf9},
   {0xc2,0xca,0xd2,0xda,0xe2,0xea,0xf2,0xfa},
   {0xc3,0xcb,0xd3,0xdb,0xe3,0xeb,0xf3,0xfb},
   {0xc4,0xcc,0xd4,0xdc,0xe4,0xec,0xf4,0xfc},
   {0xc5,0xcd,0xd5,0xdd,0xe5,0xed,0xf5,0xfd},
   {0xc6,0xce,0xd6,0xde,0xe6,0xee,0xf6,0xfe},
   {0xc7,0xcf,0xd7,0xdf,0xe7,0xef,0xf7,0xff}};

// Table 2.3 Intel Manual Vol 2A 2-6
// (Infuriatingly, these are transposes of the mod_rm_tables)
unsigned char sib_00_[8][8]
	{{0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07},
   {0x08,0x09,0x0a,0x0b,0x0c,0x0d,0x0e,0x0f},
	 {0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17},
   {0x18,0x19,0x1a,0x1b,0x1c,0x1d,0x1e,0x1f},
	 {0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27},
   {0x28,0x29,0x2a,0x2b,0x2c,0x2d,0x2e,0x2f},
	 {0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37},
   {0x38,0x39,0x3a,0x3b,0x3c,0x3d,0x3e,0x3f}};
unsigned char sib_01_[8][8]
	{{0x40,0x41,0x42,0x43,0x44,0x45,0x46,0x47},
   {0x48,0x49,0x4a,0x4b,0x4c,0x4d,0x4e,0x4f},
	 {0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57},
   {0x58,0x59,0x5a,0x5b,0x5c,0x5d,0x5e,0x5f},
	 {0x60,0x61,0x62,0x63,0x64,0x65,0x66,0x67},
   {0x68,0x69,0x6a,0x6b,0x6c,0x6d,0x6e,0x6f},
	 {0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77},
   {0x78,0x79,0x7a,0x7b,0x7c,0x7d,0x7e,0x7f}};
unsigned char sib_10_[8][8]
	{{0x80,0x81,0x82,0x83,0x84,0x85,0x86,0x87},
   {0x88,0x89,0x8a,0x8b,0x8c,0x8d,0x8e,0x8f},
	 {0x90,0x91,0x92,0x93,0x94,0x95,0x96,0x97},
   {0x98,0x99,0x9a,0x9b,0x9c,0x9d,0x9e,0x9f},
	 {0xa0,0xa1,0xa2,0xa3,0xa4,0xa5,0xa6,0xa7},
   {0xa8,0xa9,0xaa,0xab,0xac,0xad,0xae,0xaf},
	 {0xb0,0xb1,0xb2,0xb3,0xb4,0xb5,0xb6,0xb7},
   {0xb8,0xb9,0xba,0xbb,0xbc,0xbd,0xbe,0xbf}};
unsigned char sib_11_[8][8]
	{{0xc0,0xc1,0xc2,0xc3,0xc4,0xc5,0xc6,0xc7},
   {0xc8,0xc9,0xca,0xcb,0xcc,0xcd,0xce,0xcf},
	 {0xd0,0xd1,0xd2,0xd3,0xd4,0xd5,0xd6,0xd7},
   {0xd8,0xd9,0xda,0xdb,0xdc,0xdd,0xde,0xdf},
	 {0xe0,0xe1,0xe2,0xe3,0xe4,0xe5,0xe6,0xe7},
   {0xe8,0xe9,0xea,0xeb,0xec,0xed,0xee,0xef},
	 {0xf0,0xf1,0xf2,0xf3,0xf4,0xf5,0xf6,0xf7},
   {0xf8,0xf9,0xfa,0xfb,0xfc,0xfd,0xfe,0xff}};

inline void emit(unsigned char* buf, size_t& pos, unsigned char c) {
	buf[pos++] = c;
}

inline void emit_opcode(unsigned char* buf, size_t& pos, unsigned char c) {
	emit(buf, pos, c);;
}

inline void emit_opcode(unsigned char* buf, size_t& pos, unsigned char c, 
		                    GpReg delta) {
	emit(buf, pos, c + (0x7 & delta));
}

inline void emit_opcode(unsigned char* buf, size_t& pos, unsigned char c1, 
		                    unsigned char c2) {
	emit(buf, pos, c1);
	emit(buf, pos, c2);
}

inline void emit_opcode(unsigned char* buf, size_t& pos, unsigned char c1, 
		                    unsigned char c2, GpReg delta) {
	emit(buf, pos, c1);
	emit(buf, pos, c2 + (0x7 & delta));
}

inline void emit_opcode(unsigned char* buf, size_t& pos, unsigned char c1, 
												unsigned char c2, unsigned char c3) {
	emit(buf, pos, c1);
	emit(buf, pos, c2);
	emit(buf, pos, c3);
}

inline void emit_opcode(unsigned char* buf, size_t& pos, unsigned char c1, 
		                    unsigned char c2, unsigned char c3, GpReg delta) {
	emit(buf, pos, c1);
	emit(buf, pos, c2);
	emit(buf, pos, c3 + (0x7 & delta));
}

// The following four methods are used for both immediates and displacements.
// Integer constants are written in reverse byte order.
inline void emit_byte(unsigned char* buf, size_t& pos, Imm imm) {
	emit(buf, pos, imm & 0xff); 
}

inline void emit_word(unsigned char* buf, size_t& pos, Imm imm) {
	emit(buf, pos, (imm &   0xff) >> 0);
	emit(buf, pos, (imm & 0xff00) >> 8);
}

inline void emit_double(unsigned char* buf, size_t& pos, Imm imm) {
	emit(buf, pos, (imm &       0xff) >> 0);
	emit(buf, pos, (imm &     0xff00) >> 8);
	emit(buf, pos, (imm &   0xff0000) >> 16);
	emit(buf, pos, (imm & 0xff000000) >> 24);
}

inline void emit_quad(unsigned char* buf, size_t& pos, Imm imm) {
	emit(buf, pos, (imm &               0xff) >> 0);
	emit(buf, pos, (imm &             0xff00) >> 8);
	emit(buf, pos, (imm &           0xff0000) >> 16);
	emit(buf, pos, (imm &         0xff000000) >> 24);
	emit(buf, pos, (imm &       0xff00000000) >> 32);
	emit(buf, pos, (imm &     0xff0000000000) >> 40);
	emit(buf, pos, (imm &   0xff000000000000) >> 48);
	emit(buf, pos, (imm & 0xff00000000000000) >> 56);
}

// MOD R/M nop -- Simplifies codegen.
// This corresponds to instructions without any explicit operands.
// Calls to this method should be inlined away.
inline void emit_mod_rm(unsigned char* buf, size_t& pos) {
}

// MOD R/M nop -- Simplifies codegen.
// This corresponds to instructions with a single operand and no digit bit.
// This is the class of instructions that encode operand directly in opcode.
// See bswap for example.
// Calls to this method should be inlined away.
inline void emit_mod_rm(unsigned char* buf, size_t& pos, Operand ignore) {
}

// This ignores the distinction between high and low general purpose regs,
//   It won't work correctly for AH, BH, CH, DH
inline void emit_mod_rm(unsigned char* buf, size_t& pos, Operand rm, 
		 										Operand r) {
	emit(buf, pos, mod_rm_11_[rm & 0x7][r & 0x7]);
}

// This ignores the distinction between high and low general purpose regs,
//   It won't work correctly for AH, BH, CH, DH
inline void emit_mod_rm(unsigned char* buf, size_t& pos, Addr rm, Operand r) {
	auto base  = rm.get_base() & 0x7;
	auto index = rm.get_index();
	const auto disp  = rm.get_disp();

	// Step 1: Emit the MOD R/M byte.
	if ( disp == 0 ) {
		// This space is rededicated to the rip/eip+disp32 address type.
		// Seems like we jump down to the 01 row in this case.
		if ( base == 0x5 )
			emit(buf, pos, mod_rm_01_[index.is_null() ? base : 0x4][r & 0x7]);
		else
			emit(buf, pos, mod_rm_00_[index.is_null() ? base : 0x4][r & 0x7]);
	}
	else if ( disp <= 0xff )
		emit(buf, pos, mod_rm_01_[index.is_null() ? base : 0x4][r & 0x7]);
	else
		emit(buf, pos, mod_rm_10_[index.is_null() ? base : 0x4][r & 0x7]);

	// Step 2: Emit the optional SIB byte.
	//   This happens either for scaled indices or for RSP/R12D above.
	if ( base == 0x4 || !index.is_null() ) {
		const auto idx = index.is_null() ? 0x4 : index & 0x7;
		switch ( rm.get_scale() ) {
			case TIMES_1: emit(buf, pos, sib_00_[idx][base]); break;
			case TIMES_2: emit(buf, pos, sib_01_[idx][base]); break;
			case TIMES_4: emit(buf, pos, sib_10_[idx][base]); break;
			case TIMES_8: emit(buf, pos, sib_11_[idx][base]); break;
			default:
				assert(false);
		}
	}

	// Step 3: Emit displacement
	if ( disp == 0 ) {
		if ( base == 0x5 )
			emit(buf, pos, 0x0);
	}
	else if ( disp <= 0xff )
		emit_byte(buf, pos, disp);
	else
		emit_double(buf, pos, disp);
}

// REX nop -- Simplifies codegen.
// Calls to this method should be inlined away.
inline void emit_rex(unsigned char* buf, size_t& pos) {
}

// REX nop -- Simplifies codegen.
// Calls to this method should be inlined away.
inline void emit_rex(unsigned char* buf, size_t& pos, unsigned char rex) {
}

// Figure 2.7: Intel Manual Vol 2A 2-9
// This ignores the distinction between high and low general purpose regs,
//   but that's fine because it wouldn't get you an rex.b either way.
inline void emit_rex(unsigned char* buf, size_t& pos, Operand rm, 
		                 unsigned char rex) {
	if ( rm & 0x8 )
		rex |= 0x41;
	if ( rex )
		emit(buf, pos, rex);
}

// Figure 2.5: Intel Manual Vol 2A 2-8
// This ignores the distinction between high and low general purpose regs,
//   but that's fine because it wouldn't get you an rex.b either way.
inline void emit_rex(unsigned char* buf, size_t& pos, Operand rm, Operand r, 
		                 unsigned char rex) {
	if ( r & 0x8 )
		rex |= 0x44;
	if ( rm & 0x8 )
		rex |= 0x41;
	if ( rex )
		emit(buf, pos, rex);
}

// Figure 2.4 & 2.6: Intel Manual Vol 2A 2-8 & 2.9
// This ignores the distinction between high and low general purpose regs,
//   but that's fine because it wouldn't get you an rex.b either way.
inline void emit_rex(unsigned char* buf, size_t& pos, Addr rm, Operand r, 
		                 unsigned char rex) {
	if ( r & 0x8 )
		rex |= 0x44;
	if ( rm.get_base() & 0x8 )
		rex |= 0x41;
	// Note: This relies on GP_REG_NUL == 16
	if ( rm.get_index() & 0x8 )
		rex |= 0x42;
	if ( rex )
		emit(buf, pos, rex);
}

// This is essentially identical to the case above.
// The only difference being that there is no possibility of setting rex.b.
inline void emit_rex(unsigned char* buf, size_t& pos, Addr rm, 
		                 unsigned char rex) {
	if ( rm.get_base() & 0x8 )
		rex |= 0x41;
	// Note: This relies on GP_REG_NUL == 16
	if ( rm.get_index() & 0x8 )
		rex |= 0x42;
	if ( rex )
		emit(buf, pos, rex);
}

} // namespace

namespace x64 {

void Assembler::start(Function& fxn) {
	start(fxn.buffer_);
}

void Assembler::assemble(const Instruction& i) {
	switch ( i.get_opcode() ) {
		case LABEL_DEFN_64L:
			labels_[i.get_label(0)] = pos_;
			break;

		// 4000-way switch
		#include "src/gen/assembler.switch"
		
		default:
			assert(false);
			emit(buf_, pos_, 0x90);
	}
}

void Assembler::finish() {
	for ( const auto& jump : jumps_ ) {
		cerr << "[" << jump.first << "->" << jump.second << "]" << endl;

		auto pos = jump.first;
		const auto itr = labels_.find(jump.second);
		
		if ( itr == labels_.end() ) {
			emit(buf_, pos, 0x0);
			emit(buf_, pos, 0x0);
			emit(buf_, pos, 0x0);
			emit(buf_, pos, 0x0);
		}
		else
			emit_double(buf_, pos, itr->second-pos-4);
	}
}

// void Assembler::adcb(Imm arg0) { } ...
#include "src/gen/assembler.defn"

void Assembler::write_binary(ostream& os, const Code& code) {
	unsigned char buffer[1024];
	start(buffer);

	for ( const auto& instr : code )
		assemble(instr);
	finish();

	for ( size_t i = 0; i < pos_; ++i )
		os << buf_[i];
}

void Assembler::write_hex(ostream& os, const Code& code) {
	unsigned char buffer[1024];
	start(buffer);

	set<size_t> line_breaks;
	for ( const auto& instr : code ) {
		assemble(instr);
		line_breaks.insert(pos_);
	}
	finish();

	for ( size_t i = 0; i < pos_; ++i ) {
		if ( line_breaks.find(i) != line_breaks.end() )
			os << endl;
		os << hex << setfill('0') << setw(2) << (int) buf_[i] << " ";
	}
}

void Assembler::start(unsigned char* buffer) {
	labels_.clear();
	jumps_.clear();

	pos_ = 0;
	buf_ = buffer;
}



} // namespace x64
