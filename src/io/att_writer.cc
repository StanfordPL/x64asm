#include "src/io/att_writer.h"

#include <cassert>
#include <vector>

#include "src/code/attributes.h"

using namespace std;

namespace {

vector<const char*> opcode_ {{
	// Internal mnemonics
	"<label definition>"
	// Auto-generated mnemonics
	#include "src/io/opcode.att"
}};	

}

namespace x64 {

void AttWriter::write(ostream& os, const Code& c) {
	for ( const auto& instr : c ) {
		write(os, instr);
		os << endl;
	}
}

void AttWriter::write(ostream& os, const Instruction& instr) {
	write(os, instr.get_opcode());
	os << " ";

	for ( size_t i = Attributes::arity(instr)-1; i >= 0; ++i ) {
		const auto o = instr.get_operand(i);

		#define WRITE(T) write(os, (T)o);
		switch ( Attributes::type(instr, i) ) {
			case OpType::CR:         
			case OpType::CR_0234:    
			case OpType::CR_8:       WRITE(Cr);
			case OpType::DR:         WRITE(Dr);
			case OpType::EFLAG:      WRITE(Eflag);
			case OpType::IMM:        
			case OpType::IMM_8:      
			case OpType::IMM_16:     
			case OpType::IMM_32:     
			case OpType::IMM_64:     
			case OpType::ZERO:       
			case OpType::ONE:        
			case OpType::THREE:      WRITE(Imm);
			case OpType::LABEL:      WRITE(Label);
			case OpType::M:
			case OpType::M_8:
			case OpType::M_16:
			case OpType::M_32:
			case OpType::M_64:
			case OpType::M_128:
			case OpType::M_256:
			case OpType::M_PAIR_16_64:
			case OpType::M_PTR_16_16:
			case OpType::M_PTR_16_32:
			case OpType::M_PTR_16_64:
			case OpType::M_16_INT:
			case OpType::M_32_INT:
			case OpType::M_64_INT:
			case OpType::M_32_FP:
			case OpType::M_64_FP:
			case OpType::M_80_FP:
			case OpType::M_2_BYTE:
			case OpType::M_14_BYTE:
			case OpType::M_28_BYTE:
			case OpType::M_94_BYTE:
			case OpType::M_108_BYTE:
			case OpType::M_512_BYTE: WRITE(M);
			case OpType::MM:         WRITE(Mm);
			case OpType::MODIFIER:   
			case OpType::PREF_66:    
			case OpType::PREF_REX_W: 
			case OpType::FAR:        continue;
			case OpType::MOFFS:      
			case OpType::MOFFS_8:
			case OpType::MOFFS_16:
			case OpType::MOFFS_32:
			case OpType::MOFFS_64:   WRITE(Moffs);
			case OpType::R:          assert(false);
			case OpType::NO_REX_R8:  WRITE(NoRexR8);
			case OpType::REX_R8:     WRITE(RexR8);
			case OpType::RH:         WRITE(Rh);
			case OpType::RL:         
			case OpType::AL:         
			case OpType::CL:         WRITE(Rl);
			case OpType::RB:         WRITE(Rb);
			case OpType::R_16:       
			case OpType::AX:         
			case OpType::DX:         WRITE(R16);
			case OpType::R_32:       
			case OpType::EAX:        WRITE(R32);
			case OpType::R_64:       
			case OpType::RAX:        WRITE(R64);
			case OpType::REL:        
			case OpType::REL_8:      
			case OpType::REL_32:     WRITE(Rel);
			case OpType::SREG:       
			case OpType::FS:         
			case OpType::GS:         WRITE(Sreg);
			case OpType::XMM:        
			case OpType::XMM_0:      WRITE(Xmm);
			case OpType::YMM:        WRITE(Ymm);
			default: assert(false);
		}
		#undef WRITE

		if ( i != 0 )
			os << ", ";
	}
}	

void AttWriter::write(ostream& os, const Opcode o) {
	assert(o < opcode_.size());
	os << opcode_[o];
}

void AttWriter::write(ostream& os, const Cr c) {
	switch ( c.val_ ) {
		case 0: os << "%cr0"; break;
		case 2: os << "%cr2"; break;
		case 3: os << "%cr3"; break;
		case 4: os << "%cr4"; break;
		case 8: os << "%cr8"; break;

		default: assert(false);
	}				 
}

void AttWriter::write(ostream& os, const Dr d) {
	assert(d.val_ < 8);
	os << "%dr" << dec << d.val_;
}

void AttWriter::write(ostream& os, const Eflag e) {
	switch ( e.val_ ) {
		case 0: os << "%cf"; break;
		case 2: os << "%pf"; break;
		case 4: os << "%af"; break;
		case 6: os << "%zf"; break;
		case 7: os << "%sf"; break;
		case 8: os << "%tf"; break;
		case 9: os << "%if"; break;
		case 10: os << "%dr"; break;
		case 11: os << "%of"; break;
		case 12: os << "%iopl[0]"; break;
		case 13: os << "%iopl[1]"; break;
		case 14: os << "%nt"; break;
		case 16: os << "%rf"; break;
		case 17: os << "%vm"; break;
		case 18: os << "%ac"; break;
		case 19: os << "%vif"; break;
		case 20: os << "%vip"; break;
		case 21: os << "%id"; break;

		default: assert(false);
	}
}

void AttWriter::write(ostream& os, const Imm i) {
	os << "$" << hex << i.val_;
}

void AttWriter::write(ostream& os, const Label l) {
	os << ".LABEL_" << dec << l.val_;
}

void AttWriter::write(ostream& os, const M m) {
	if ( !m.null_seg() ) {
		write(os, m.get_seg());
		os << ":";
	}
	os << "(";
	if ( !m.null_base() ) {
		const auto b = m.get_base();
		if ( m.get_addr_or() )
			write(os, (R32)b);
		else
			write(os, (R64)b);
	}
	if ( !m.null_base() && !m.null_index() )
		os << ",";
	if ( !m.null_index() ) {
		const auto i = m.get_index();
		if ( m.get_addr_or() )
			write(os, (R32)i);
		else
			write(os, (R64)i);
		os << ",";
		switch ( m.get_scale() ) {
			case Scale::TIMES_1: os << "1"; break;
			case Scale::TIMES_2: os << "2"; break;
			case Scale::TIMES_4: os << "4"; break;
			case Scale::TIMES_8: os << "8"; break;
			default: assert(false);
		}
	}
	os << ")";
}

void AttWriter::write(ostream& os, const Mm m) {
	assert(m.val_ < 8);
	os << "%mm" << dec << m.val_;
}

void AttWriter::write(ostream& os, const Moffs m) {
	os << hex << showbase << m.val_;
}

void AttWriter::write(ostream& os, const NoRexR8 r) {
	if ( r.val_ < 4 )
		write(os, (Rl)r);
	else if ( r.val_ < 8 )
		write(os, (Rh)r);
	else
		assert(false);
}

void AttWriter::write(ostream& os, const RexR8 r) {
	if ( r.val_ < 4 )
		write(os, (Rl)r);
	else if ( r.val_ < 16 )
		write(os, (Rb)r);
	else
		assert(false);
}

void AttWriter::write(ostream& os, const Rl r) {
	switch ( r.val_ ) {
		case 0:  os << "%al"; break;
		case 1:  os << "%cl"; break;
		case 2:  os << "%dl"; break;
		case 3:  os << "%bl"; break;

		default: assert(false);
	}
}

void AttWriter::write(ostream& os, const Rh r) {
	switch ( r.val_ ) {
		case 4:  os << "%ah"; break;
		case 5:  os << "%ch"; break;
		case 6:  os << "%dh"; break;
		case 7:  os << "%bh"; break;

		default: assert(false);
	}
}

void AttWriter::write(ostream& os, const Rb r) {
	switch ( r.val_ ) {
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

		default: assert(false);
	}
}

void AttWriter::write(ostream& os, const R16 r) {
	switch ( r.val_ ) {
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

		default: assert(false);
	}
}

void AttWriter::write(ostream& os, const R32 r) {
	switch ( r.val_ ) {
		case 0:  os << "%eax"; break;
		case 1:  os << "%ecx"; break;
		case 2:  os << "%edx"; break;
		case 3:  os << "%ebx"; break;
		case 4:  os << "%esp"; break;
		case 5:  os << "%ebp"; break;
		case 6:  os << "%esi"; break;
		case 7:  os << "%edi"; break;
		case 8:  os << "%r8d"; break;
		case 9:  os << "%r9d"; break;
		case 10: os << "%r10d"; break;
		case 11: os << "%r11d"; break;
		case 12: os << "%r12d"; break;
		case 13: os << "%r13d"; break;
		case 14: os << "%r14d"; break;
		case 15: os << "%r15d"; break;

		default: assert(false);
	}
}

void AttWriter::write(ostream& os, const R64 r) {
	switch ( r.val_ ) {
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

		default: assert(false);
	}
}

void AttWriter::write(ostream& os, const Rel r) {
	os << hex << showbase << r.val_;
}

void AttWriter::write(ostream& os, const Sreg s) {
	switch ( s.val_ ) {
		case 0: os << "%es"; break;
		case 1: os << "%cs"; break;
		case 2: os << "%ss"; break;
		case 3: os << "%ds"; break;
		case 4: os << "%fs"; break;
		case 5: os << "%gs"; break;

		default: assert(false);
	}
}

void AttWriter::write(ostream& os, const St s) {
	assert(s.val_ < 8);
	if ( s.val_ == 0 )
		os << "%st";	
	else
		os << "%st(" << dec << s.val_ << ")";
}

void AttWriter::write(ostream& os, const Xmm x) {
	assert(x.val_ < 16);
	os << "%xmm" << dec << x.val_;
}

void AttWriter::write(ostream& os, const Ymm y) {
	assert(y.val_ < 16);
	os << "%ymm" << dec << y.val_;
}

void AttWriter::write(ostream& os, const OpSet& o) {
	os << "{ ";
	for ( uint64_t r = 0; r < 16; ++r ) {
		if ( o.contains((R64)r) )
			write(os, (R64)r);
		else if ( o.contains((R32)r) )
			write(os, (R32)r);
		else if ( o.contains((R16)r) )
			write(os, (R16)r);
		else if ( r < 4 && o.contains((Rh)(r+4)) )
				write(os, (Rh)r);
		else if ( o.contains((Rl)r) )
			write(os, (Rl)r);
		else 
			os << ".";
		os << " ";
	}
	os << "}" << endl;

	os << "{ ";
	for ( uint64_t x = 0; x < 16; ++x ) {
		if ( o.contains((Ymm)x) )
			write(os, (Ymm)x);
		else if ( o.contains((Xmm)x) )
			write(os, (Xmm)x);
		else
			os << ".";
		os << " ";
	}
	os << "}" << endl;

	os << "{ ";
	for ( uint64_t m = 0; m < 8; ++m ) {
		if ( o.contains((Mm)m) ) 
			write(os, (Mm)m);
		else
			os << ".";
		os << " ";
	}
	os << "}";
}

} // namespace x64
