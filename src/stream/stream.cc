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

ostream& operator<<(ostream& os, const Instruction& instr) {
	if ( get_format(os) == ATT ) {
		AttWriter w;
		w.write(os, instr);
	}
	else
		os.setstate(ios::failbit);
	return os;
}
