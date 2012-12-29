#include "src/io/intel_writer.h"

#include <cassert>

using namespace std;

namespace x64 {

void IntelWriter::write(ostream& os, const Code& c) {
	for ( const auto& instr : c ) {
		write(os, instr);
		os << endl;
	}
}

void IntelWriter::write(ostream& os, const Instruction& i) {
	// TODO...
}	

void IntelWriter::write(ostream& os, const Opcode o) {
	// TODO...
}

void IntelWriter::write(ostream& os, Cr c) {
	// TODO...
}

void IntelWriter::write(ostream& os, Dr d) {
	// TODO...
}

void IntelWriter::write(ostream& os, Eflag e) {
	// TODO...
}

void IntelWriter::write(ostream& os, Imm i) {
	// TODO...
}

void IntelWriter::write(ostream& os, Label l) {
	// TODO...
}

void IntelWriter::write(ostream& os, M m) {
	// TODO...
}

void IntelWriter::write(ostream& os, Mm m) {
	// TODO...
}

void IntelWriter::write(ostream& os, Moffs m) {
	// TODO...
}

void IntelWriter::write(ostream& os, Rl r) {
	// TODO...
}

void IntelWriter::write(ostream& os, Rh r) {
	// TODO...
}

void IntelWriter::write(ostream& os, Rb r) {
	// TODO...
}

void IntelWriter::write(ostream& os, R16 r) {
	// TODO...
}

void IntelWriter::write(ostream& os, R32 r) {
	// TODO...
}

void IntelWriter::write(ostream& os, R64 r) {
	// TODO...
}

void IntelWriter::write(ostream& os, Rel r) {
	// TODO...
}

void IntelWriter::write(ostream& os, Sreg s) {
	// TODO...
}

void IntelWriter::write(ostream& os, St s) {
	// TODO...
}

void IntelWriter::write(ostream& os, Xmm x) {
	// TODO...
}

void IntelWriter::write(ostream& os, Ymm y) {
	// TODO...
}

} // namespace x64
