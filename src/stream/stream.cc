#include "src/stream/stream.h"

#include "src/assembler/assembler.h"
#include "src/cfg/control_flow_graph.h"
#include "src/att/att_reader.h"
#include "src/att/att_writer.h"

using namespace std;
using namespace x64;

namespace {

inline int state_i() {
	static int i = ios_base::xalloc();
	return i;
}

template <typename T>
inline FormatVal get_format(T& ios) {
	return (FormatVal) ios.iword(state_i());
}

template <typename T>
ostream& generic_write(ostream& os, const T t) {
	if ( get_format(os) == ATT ) {
		AttWriter w;
		w.write(os, t);
	}
	else
		os.setstate(ios::failbit);
	return os;
}

}

istream& operator>>(istream& is, const format& f) {
	is.iword(state_i()) = f;
	return is;
}

ostream& operator<<(ostream& os, const format& f) {
	os.iword(state_i()) = f;
	return os;
}

istream& operator>>(istream& is, Code& c) {
	if ( get_format(is) == ATT ) {
		AttReader r;
		r.read(is, c);
	}
	else
		is.setstate(ios::failbit);
	return is;
}

ostream& operator<<(ostream& os, const Code& c) {
	const auto format = get_format(os);
	if ( format == ATT ) {
		AttWriter w;
		w.write(os, c);
	}
	else if ( format == BIN ) {
		Assembler assm;
		assm.write_binary(os, c);
	}
	else if ( format == DOT ) {
		ControlFlowGraph cfg(c);
		cfg.write_dot(os);
	}
	else if ( format == HEX ) {
		Assembler assm;
		assm.write_hex(os, c);
	}
	else
		os.setstate(ios::failbit);

	return os;
}

ostream& operator<<(ostream& os, x64::CondReg cr) {
	return generic_write(os, cr);
}

ostream& operator<<(ostream& os, x64::Imm8 i) {
	return generic_write(os, i);
}

ostream& operator<<(ostream& os, x64::Imm16 i) {
	return generic_write(os, i);
}

ostream& operator<<(ostream& os, x64::Imm32 i) {
	return generic_write(os, i);
}

ostream& operator<<(ostream& os, x64::Imm64 i) {
	return generic_write(os, i);
}

ostream& operator<<(ostream& os, const x64::Instruction& instr) {
	return generic_write(os, instr);
}

ostream& operator<<(ostream& os, x64::Label l) {
	return generic_write(os, l);
}

ostream& operator<<(ostream& os, x64::M m) {
	return generic_write(os, m);
}

ostream& operator<<(ostream& os, x64::Mm m) {
	return generic_write(os, m);
}

ostream& operator<<(ostream& os, x64::Moffs o) {
	return generic_write(os, o);
}

ostream& operator<<(ostream& os, x64::Opcode o) {
	return generic_write(os, o);
}

ostream& operator<<(ostream& os, x64::RH r) {
	return generic_write(os, r);
}

ostream& operator<<(ostream& os, x64::R8 r) {
	return generic_write(os, r);
}

ostream& operator<<(ostream& os, x64::R16 r) {
	return generic_write(os, r);
}

ostream& operator<<(ostream& os, x64::R32 r) {
	return generic_write(os, r);
}

ostream& operator<<(ostream& os, x64::R64 r) {
	return generic_write(os, r);
}

ostream& operator<<(ostream& os, x64::Scale s) {
	return generic_write(os, s);
}

ostream& operator<<(ostream& os, x64::Sreg s) {
	return generic_write(os, s);
}

ostream& operator<<(ostream& os, x64::St st) {
	return generic_write(os, st);
}

ostream& operator<<(ostream& os, x64::Xmm x) {
	return generic_write(os, x);
}

ostream& operator<<(ostream& os, x64::Ymm y) {
	return generic_write(os, y);
}

