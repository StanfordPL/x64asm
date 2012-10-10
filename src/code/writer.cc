#include "src/code/writer.h"

#include <cassert>

using namespace std;

namespace {

// const char* opcodes_[] {{ ... }}
#include "src/gen/opcode.char"

}

namespace x64 {

void Writer::write_att(ostream& os, Addr addr) const {
	const auto w = addr.get_size_or() ? DOUBLE : QUAD;
	
	const auto s = addr.get_seg();
	if ( !s.is_null() ) {
		write_att(os, s);
		os << ":";
	}

	const auto d = (int32_t) addr.get_disp();
	if ( d > 0 )
		os << hex << showbase << d;
	else if ( d < 0 )
		os << "-" << hex << showbase << (-1 * d);

	os << "(";
 	write_att(os, addr.get_base(), w);
	const auto i = addr.get_index();
	if ( !i.is_null() ) {
		os << ",";
	 	write_att(os, i, w);
	}	
	const auto sc = addr.get_scale();
	if ( !sc.is_null() ) {
		os << ",";
	 	write_att(os, sc);
	}
	os << ")";
}

void Writer::write_att(ostream& os, const Code& code) const {
	for ( const auto& instr : code ) {
		write_att(os, instr);
		os << endl;
	}
}

void Writer::write_att(ostream& os, FpReg fp) const {
	switch ( fp ) {
		case 0: os << "%st";    break;
		case 1: os << "%st(1)"; break;
		case 2: os << "%st(2)"; break;
		case 3: os << "%st(3)"; break;
		case 4: os << "%st(4)"; break;
		case 5: os << "%st(5)"; break;
		case 6: os << "%st(6)"; break;
		case 7: os << "%st(7)"; break;

		default:
			assert(false);
	}
}

void Writer::write_att(ostream& os, GpReg gp, BitWidth w) const {
	switch ( gp ) {
		#define ABCD(R,Q,D,W,H,L)                \
			case R: switch ( w ) {                 \
								case QUAD:   os << Q; break; \
				  			case DOUBLE: os << D; break; \
				  			case WORD:   os << W; break; \
								case HIGH:   os << H; break; \
								case LOW:    os << L; break; \
								default:                     \
									os.setstate(ios::failbit); \
							}                              \
							break;
		ABCD(0, "%rax", "%eax", "%ax", "%ah", "%al")
		ABCD(1, "%rcx", "%ecx", "%cx", "%ch", "%cl")
		ABCD(2, "%rdx", "%edx", "%dx", "%dh", "%dl")
		ABCD(3, "%rbx", "%ebx", "%bx", "%bh", "%bl")
		#undef ABCD

		#define OTH(R,Q,D,W,L)                   \
			case R: switch ( w ) {                 \
								case QUAD:   os << Q; break; \
				  			case DOUBLE: os << D; break; \
				  			case WORD:   os << W; break; \
								case LOW:    os << L; break; \
								default:                     \
									os.setstate(ios::failbit); \
							}                              \
							break;
		OTH(4,  "%rsp", "%esp",  "%sp",   "%spl")
		OTH(5,  "%rbp", "%ebp",  "%bp",   "%bpl")
		OTH(6,  "%rsi", "%esi",  "%si",   "%sil")
		OTH(7,  "%rdi", "%edi",  "%di",   "%dil")
		OTH(8,  "%r8",  "%r8d",  "%r8w",  "%r8b")
		OTH(9,  "%r9",  "%r9d",  "%r9w",  "%r9b")
		OTH(10, "%r10", "%r10d", "%r10w", "%r10b")
		OTH(11, "%r11", "%r11d", "%r11w", "%r11b")
		OTH(12, "%r12", "%r12d", "%r12w", "%r12b")
		OTH(13, "%r13", "%r13d", "%r13w", "%r13b")
		OTH(14, "%r14", "%r14d", "%r14w", "%r14b")
		OTH(15, "%r15", "%r15d", "%r15w", "%r15b")
		#undef OTH

		default:
			assert(false);
	}
}

void Writer::write_att(ostream& os, Imm i, BitWidth w) const {
	os << hex << showbase << "$";
	switch ( w ) {
		case LOW:    os << (i & 0x00000000000000ff); break;
		case WORD:   os << (i & 0x000000000000ffff); break;
		case DOUBLE: os << (i & 0x00000000ffffffff); break;
		case QUAD:   os << i; break;

		default:		 
			assert(false);
	}
}

void Writer::write_att(ostream& os, const Instruction& instr) const {
	const auto opc = instr.get_opcode();

	if ( opc.is_label_defn() ) {
		write_att(os, instr.get_label(0));
		os << ":";
		return;
	}

	write_att(os, opc);
	os << " ";

	auto comma = false;
	for ( int i = instr.arity()-1; i >= 0; --i ) {
		if ( comma )
			os << ",";
		else
			comma = true;

		switch ( instr.type(i) ) {
			case ADDR:     write_att(os, instr.get_addr(i));
										 break;
			case GP_REG:  
			case RAX_ONLY:
			case RCX_ONLY: write_att(os, instr.get_gp_reg(i), instr.width(i)); 
										 break;
			case IMM:      write_att(os, instr.get_imm(i), instr.width(i)); 
										 break;
			case XMM_REG:  write_att(os, instr.get_xmm_reg(i)); 
										 break;
			case LABEL:    write_att(os, instr.get_label(i));
										 break;
			case OFFSET:   write_att(os, instr.get_offset(i));
										 break;

			default:      
				assert(false);							 
		}
	}
}

void Writer::write_att(ostream& os, Label l) const {
	os << ".L" << dec << l;
}

void Writer::write_att(ostream& os, Offset o) const {
	os << hex << showbase << o;
}

void Writer::write_att(ostream& os, MmxReg mm) const {
	switch ( mm ) {
		case 0: os << "%mm0"; break;
		case 1: os << "%mm1"; break;
		case 2: os << "%mm2"; break;
		case 3: os << "%mm3"; break;
		case 4: os << "%mm4"; break;
		case 5: os << "%mm5"; break;
		case 6: os << "%mm6"; break;
		case 7: os << "%mm7"; break;

		default:
			assert(false);
	}	
}

void Writer::write_att(ostream& os, Opcode o) const {
	assert(!o.is_label_defn());
	assert(!o.is_null());
	os << opcodes_[o];
}

void Writer::write_att(ostream& os, Scale s) const {
	switch ( s ) {
		case 0: os << "1"; break;
		case 1: os << "2"; break;
		case 2: os << "4"; break;
		case 3: os << "8"; break;
		
		default: 
			assert(false);
	}
}

void Writer::write_att(ostream& os, SegReg seg) const {
	switch ( seg ) {
		case 0: os << "es"; break;
		case 1: os << "cs"; break;
		case 2: os << "ss"; break;
		case 3: os << "ds"; break;
		case 4: os << "fs"; break;
		case 5: os << "gs"; break;

		default: 
			assert(false);
	}
}

void Writer::write_att(ostream& os, XmmReg xmm) const {
	switch ( xmm ) {
		case  0: os << "%xmm0";  break;
		case  1: os << "%xmm1";  break;
		case  2: os << "%xmm2";  break;
		case  3: os << "%xmm3";  break;
		case  4: os << "%xmm4";  break;
		case  5: os << "%xmm5";  break;
		case  6: os << "%xmm6";  break;
		case  7: os << "%xmm7";  break;
		case  8: os << "%xmm8";  break;
		case  9: os << "%xmm9";  break;
		case 10: os << "%xmm10"; break;
		case 11: os << "%xmm11"; break;
		case 12: os << "%xmm12"; break;
		case 13: os << "%xmm13"; break;
		case 14: os << "%xmm14"; break;
		case 15: os << "%xmm15"; break;

		default:
			assert(false);
	}
}

} // namespace std
