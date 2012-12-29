#include "src/io/att_writer.h"

#include <cassert>
#include <vector>

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

void AttWriter::write(ostream& os, const Instruction& i) {
	// TODO...
}	

void AttWriter::write(ostream& os, const Opcode o) {
	assert(o < opcode_.size());
	os << opcode_[o];
}

void AttWriter::write(ostream& os, Cr c) {
	switch ( c.val_ ) {
		case 0: os << "%cr0"; break;
		case 2: os << "%cr2"; break;
		case 3: os << "%cr3"; break;
		case 4: os << "%cr4"; break;
		case 8: os << "%cr8"; break;

		default: assert(false);
	}				 
}

void AttWriter::write(ostream& os, Dr d) {
	assert(d.val_ < 8);
	os << "%dr" << dec << d.val_;
}

void AttWriter::write(ostream& os, Eflag e) {
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

void AttWriter::write(ostream& os, Imm i) {
	os << "$" << hex << i.val_;
}

void AttWriter::write(ostream& os, Label l) {
	os << ".LABEL_" << dec << l.val_;
}

void AttWriter::write(ostream& os, M m) {
	// TODO...
}

void AttWriter::write(ostream& os, Mm m) {
	assert(m.val_ < 8);
	os << "%mm" << dec << m.val_;
}

void AttWriter::write(ostream& os, Moffs m) {
	os << hex << showbase << m.val_;
}

void AttWriter::write(ostream& os, Rl r) {
	switch ( r.val_ ) {
		case 0:  os << "%al"; break;
		case 1:  os << "%cl"; break;
		case 2:  os << "%dl"; break;
		case 3:  os << "%bl"; break;

		default: assert(false);
	}
}

void AttWriter::write(ostream& os, Rh r) {
	switch ( r.val_ ) {
		case 4:  os << "%ah"; break;
		case 5:  os << "%ch"; break;
		case 6:  os << "%dh"; break;
		case 7:  os << "%bh"; break;

		default: assert(false);
	}
}

void AttWriter::write(ostream& os, Rb r) {
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

void AttWriter::write(ostream& os, R16 r) {
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

void AttWriter::write(ostream& os, R32 r) {
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

void AttWriter::write(ostream& os, R64 r) {
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

void AttWriter::write(ostream& os, Rel r) {
	os << hex << showbase << r.val_;
}

void AttWriter::write(ostream& os, Sreg s) {
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

void AttWriter::write(ostream& os, St s) {
	assert(s.val_ < 8);
	if ( s.val_ == 0 )
		os << "%st";	
	else
		os << "%st(" << dec << s.val_ << ")";
}

void AttWriter::write(ostream& os, Xmm x) {
	assert(x.val_ < 16);
	os << "%xmm" << dec << x.val_;
}

void AttWriter::write(ostream& os, Ymm y) {
	assert(y.val_ < 16);
	os << "%ymm" << dec << y.val_;
}

} // namespace x64
