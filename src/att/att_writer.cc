#include "src/att/att_writer.h"

using namespace std;

namespace {

// const char* opcodes_[] {{ ... }}
#include "src/gen/opcode.char"

}

namespace x64 {

void AttWriter::write(ostream& os, const Code& code) const {
	for ( const auto& instr : code ) {
		write(os, instr);
		os << endl;
	}
}

void AttWriter::write(ostream& os, Imm8 i) const {
	os << hex << showbase << "$" << (i & 0xff);
}

void AttWriter::write(ostream& os, Imm16 i) const {
	os << hex << showbase << "$" << (i & 0xffff);
}

void AttWriter::write(ostream& os, Imm32 i) const {
	os << hex << showbase << "$" << (i & 0xffffffff);
}

void AttWriter::write(ostream& os, Imm64 i) const {
	os << hex << showbase << "$" << i;
}

void AttWriter::write(ostream& os, const Instruction& instr) const {
	const auto opc = instr.get_opcode();

	if ( opc.is_label_defn() ) {
		write(os, (Label)instr.get_operand(0));
		os << ":";
		return;
	}

	write(os, opc);
	os << " ";

	auto comma = false;
	for ( int i = instr.arity()-1; i >= 0; --i ) {
		if ( comma )
			os << ",";
		else
			comma = true;

		switch ( instr.type(i) ) {
			case R_H: write(os, (RH)instr.get_operand(i)); break;
			case AL:
			case CL:
			case R_8: write(os, (R8)instr.get_operand(i)); break;
			case AX:
			case R_16: write(os, (R16)instr.get_operand(i)); break;
			case EAX:
			case R_32: write(os, (R32)instr.get_operand(i)); break;
			case RAX:
			case R_64: write(os, (R64)instr.get_operand(i)); break;
			case IMM_8: write(os, (Imm8)instr.get_operand(i)); break;
			case IMM_16: write(os, (Imm16)instr.get_operand(i)); break;
			case IMM_32: write(os, (Imm32)instr.get_operand(i)); break;
			case IMM_64: write(os, (Imm64)instr.get_operand(i)); break;
			case LABEL: write(os, (Label)instr.get_operand(i)); break;
			case M_8:    
			case M_16:    
			case M_32:    
			case M_64:    
			case M_80:    
			case M_128:    
			case M_256: write(os, (M)instr.get_operand(i)); break;
			case MOFFS_8:		 
			case MOFFS_16:		 
			case MOFFS_32:		 
			case MOFFS_64: write(os, (Moffs)instr.get_operand(i)); break;
			case SREG: write(os, (Sreg)instr.get_operand(i)); break;
			case ST0:
			case ST: write(os, (St)instr.get_operand(i)); break;
			case MM: write(os, (Mm)instr.get_operand(i)); break;
			case XMM: write(os, (Xmm)instr.get_operand(i)); break;
			case YMM: write(os, (Ymm)instr.get_operand(i)); break;

			default:
				os << "<null>";
		}
	}
}

void AttWriter::write(ostream& os, Label l) const {
	os << ".L" << dec << l;
}

void AttWriter::write(ostream& os, M m) const {
	const auto sor = m.get_size_or();
	
	const auto s = m.get_seg();
	if ( !s.is_null() ) {
		write(os, s);
		os << ":";
	}

	const auto d = (int32_t)m.get_disp();
	if ( d > 0 )
		os << hex << showbase << d;
	else if ( d < 0 )
		os << "-" << hex << showbase << (-1 * d);

	os << "(";

	const auto b = m.get_base();
	if ( sor )
 		write(os, (R32)b);
	else
		write(os, (R64)b);
	
	const auto i = m.get_index();
	if ( !i.is_null() ) {
		os << ",";

		if ( sor )
	 		write(os, (R32)i);
		else
			write(os, (R64)i);
	}	

	const auto sc = m.get_scale();
	if ( !sc.is_null() ) {
		os << ",";
	 	write(os, sc);
	}
	os << ")";
}

void AttWriter::write(ostream& os, Mm mm) const {
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
			os << "<null>";
	}	
}

void AttWriter::write(ostream& os, Moffs o) const {
	os << hex << showbase << o;
}

void AttWriter::write(ostream& os, Opcode o) const {
	if ( o.is_label_defn() || o.is_null() )
		os << "<null>";
	else
		os << opcodes_[o >> 50];
}

void AttWriter::write(ostream& os, RH r) const {
	switch ( r ) {
		case 0: os << "%ah"; break;
		case 1: os << "%ch"; break;
		case 2: os << "%dh"; break;
		case 3: os << "%bh"; break;

		default:
			os << "<null>";
	}
}

void AttWriter::write(ostream& os, R8 r) const {
	switch ( r ) {
		case 0:  os << "%al"; break;
		case 1:  os << "%cl"; break;
		case 2:  os << "%dl"; break;
		case 3:  os << "%bl"; break;
		case 4:  os << "%spl"; break;
		case 5:  os << "%bpl"; break;
		case 6:  os << "%sil"; break;
		case 7:  os << "%dil"; break;
		case 8:  os << "%r8b"; break;
		case 9:  os << "%r9b"; break;
		case 10: os << "%r10b"; break;
		case 11: os << "%r11b"; break;
		case 12: os << "%r12b"; break;
		case 13: os << "%r13b"; break;
		case 14: os << "%r14b"; break;
		case 15: os << "%r15b"; break;

		default:
			os << "<null>";
	}
}

void AttWriter::write(ostream& os, R16 r) const {
	switch ( r ) {
		case 0:  os << "%ax"; break;
		case 1:  os << "%cx"; break;
		case 2:  os << "%dx"; break;
		case 3:  os << "%bx"; break;
		case 4:  os << "%sp"; break;
		case 5:  os << "%bp"; break;
		case 6:  os << "%si"; break;
		case 7:  os << "%di"; break;
		case 8:  os << "%r8w"; break;
		case 9:  os << "%r9w"; break;
		case 10: os << "%r10w"; break;
		case 11: os << "%r11w"; break;
		case 12: os << "%r12w"; break;
		case 13: os << "%r13w"; break;
		case 14: os << "%r14w"; break;
		case 15: os << "%r15w"; break;

		default:
			os << "<null>";
	}
}

void AttWriter::write(ostream& os, R32 r) const {
	switch ( r ) {
		case 0:  os << "%eax"; break;
		case 1:  os << "%ecx"; break;
		case 2:  os << "%edx"; break;
		case 3:  os << "%ebx"; break;
		case 4:  os << "%esp"; break;
		case 5:  os << "%ebp"; break;
		case 6:  os << "%esi"; break;
		case 7:  os << "%edi"; break;
		case 8:  os << "%r8w"; break;
		case 9:  os << "%r9w"; break;
		case 10: os << "%r10w"; break;
		case 11: os << "%r11w"; break;
		case 12: os << "%r12w"; break;
		case 13: os << "%r13w"; break;
		case 14: os << "%r14w"; break;
		case 15: os << "%r15w"; break;

		default:
			os << "<null>";
	}
}

void AttWriter::write(ostream& os, R64 r) const {
	switch ( r ) {
		case 0:  os << "%rax"; break;
		case 1:  os << "%rcx"; break;
		case 2:  os << "%rdx"; break;
		case 3:  os << "%rbx"; break;
		case 4:  os << "%rsp"; break;
		case 5:  os << "%rbp"; break;
		case 6:  os << "%rsi"; break;
		case 7:  os << "%rdi"; break;
		case 8:  os << "%r8"; break;
		case 9:  os << "%r9"; break;
		case 10: os << "%r10"; break;
		case 11: os << "%r11"; break;
		case 12: os << "%r12"; break;
		case 13: os << "%r13"; break;
		case 14: os << "%r14"; break;
		case 15: os << "%r15"; break;

		default:
			os << "<null>";
	}
}
void AttWriter::write(ostream& os, Scale s) const {
	switch ( s ) {
		case 0: os << "1"; break;
		case 1: os << "2"; break;
		case 2: os << "4"; break;
		case 3: os << "8"; break;
		
		default: 
			os << "<null>";
	}
}

void AttWriter::write(ostream& os, Sreg seg) const {
	switch ( seg ) {
		case 0: os << "es"; break;
		case 1: os << "cs"; break;
		case 2: os << "ss"; break;
		case 3: os << "ds"; break;
		case 4: os << "fs"; break;
		case 5: os << "gs"; break;

		default: 
			os << "<null>";
	}
}

void AttWriter::write(ostream& os, St st) const {
	switch ( st ) {
		case 0: os << "%st";    break;
		case 1: os << "%st(1)"; break;
		case 2: os << "%st(2)"; break;
		case 3: os << "%st(3)"; break;
		case 4: os << "%st(4)"; break;
		case 5: os << "%st(5)"; break;
		case 6: os << "%st(6)"; break;
		case 7: os << "%st(7)"; break;

		default:
			os << "<null>";
	}
}

void AttWriter::write(ostream& os, Xmm xmm) const {
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
			os << "<null>";
	}
}

void AttWriter::write(ostream& os, Ymm ymm) const {
	switch ( ymm ) {
		case  0: os << "%ymm0";  break;
		case  1: os << "%ymm1";  break;
		case  2: os << "%ymm2";  break;
		case  3: os << "%ymm3";  break;
		case  4: os << "%ymm4";  break;
		case  5: os << "%ymm5";  break;
		case  6: os << "%ymm6";  break;
		case  7: os << "%ymm7";  break;
		case  8: os << "%ymm8";  break;
		case  9: os << "%ymm9";  break;
		case 10: os << "%ymm10"; break;
		case 11: os << "%ymm11"; break;
		case 12: os << "%ymm12"; break;
		case 13: os << "%ymm13"; break;
		case 14: os << "%ymm14"; break;
		case 15: os << "%ymm15"; break;

		default:
			os << "<null>";
	}
}

} // namespace std
