#include "src/stream/stream.h"

#include "src/writer/att_writer.h"

using namespace std;
using namespace x64;

namespace {

template <typename T>
ostream& generic_write(ostream& os, const T t) {
	AttWriter::write(os, t);
	return os;
}

} // namespace

ostream& operator<<(ostream& os, const Cr c) {
	return generic_write(os, c);
}

ostream& operator<<(ostream& os, const Dr d) {
	return generic_write(os, d);
}

ostream& operator<<(ostream& os, const Eflag e) {
	return generic_write(os, e);
}

ostream& operator<<(ostream& os, const Imm i) {
	return generic_write(os, i);
}

ostream& operator<<(ostream& os, const Label l) {
	return generic_write(os, l);
}

ostream& operator<<(ostream& os, const Mm m) {
	return generic_write(os, m);
}

ostream& operator<<(ostream& os, const Moffs m) {
	return generic_write(os, m);
}

ostream& operator<<(ostream& os, const Rl r) {
	return generic_write(os, r);
}

ostream& operator<<(ostream& os, const Rh r) {
	return generic_write(os, r);
}

ostream& operator<<(ostream& os, const Rb r) {
	return generic_write(os, r);
}

ostream& operator<<(ostream& os, const R16 r) {
	return generic_write(os, r);
}

ostream& operator<<(ostream& os, const R32 r) {
	return generic_write(os, r);
}

ostream& operator<<(ostream& os, const R64 r) {
	return generic_write(os, r);
}

ostream& operator<<(ostream& os, const Rel r) {
	return generic_write(os, r);
}

ostream& operator<<(ostream& os, const Sreg s) {
	return generic_write(os, s);
}

ostream& operator<<(ostream& os, const St s) {
	return generic_write(os, s);
}

ostream& operator<<(ostream& os, const Xmm x) {
	return generic_write(os, x);
}

ostream& operator<<(ostream& os, const Ymm y) {
	return generic_write(os, y);
}

#if 0

#include "src/assembler/assembler.h"
#include "src/cfg/control_flow_graph.h"
#include "src/att/att_reader.h"
#include "src/att/att_writer.h"

using namespace std;
using namespace x64;

namespace {

inline int format_state() {
	static int i = ios_base::xalloc();
	return i;
}

template <typename T>
inline FormatVal get_format(T& ios) {
	return (FormatVal) ios.iword(format_state());
}

inline int code_format_state() {
	static int i = ios_base::xalloc();
	return i;
}

template <typename T>
inline CodeFormatVal get_code_format(T& ios) {
	return (CodeFormatVal) ios.iword(code_format_state());
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
	is.iword(format_state()) = f;
	return is;
}

ostream& operator<<(ostream& os, const format& f) {
	os.iword(format_state()) = f;
	return os;
}


ostream& operator<<(ostream& os, const code_format& f) {
	os.iword(code_format_state()) = f;
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
  const auto code_format = get_code_format(os);

  Code d;

  if ( code_format == ALL ) {
    d = c;
  } else if (code_format == NOP_REMOVED) {
    for ( const auto& line : c )
      if (line.get_opcode() != NOP)
        d.push_back(line);
  } else if (code_format == DEAD_REMOVED) {

    ControlFlowGraph cfg(c);

    for ( size_t i = cfg.get_entry(); i != cfg.get_exit(); ++i ) {
      if (cfg.is_reachable(i)) {
        for( size_t j = 0; j < cfg.num_instrs(i); j++) {
          const auto& instr = cfg.get_instr(
                  ControlFlowGraph::location_type(i,j));
          if (instr.get_opcode() != NOP)
            d.push_back(instr);
        }
      }
    }

  } else {
    os.setstate(ios::failbit);
  }
    


	if ( format == ATT ) {
		AttWriter w;
		w.write(os, d);
	}
	else if ( format == BIN ) {
		Assembler assm;
		assm.write_binary(os, d);
	}
	else if ( format == DOT ) {
		ControlFlowGraph cfg(d);
		cfg.write_dot(os);
	}
	else if ( format == HEX ) {
		Assembler assm;
		assm.write_hex(os, d);
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

#endif

