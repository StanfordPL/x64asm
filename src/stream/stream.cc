#include "src/stream/stream.h"

#include "src/assembler/assembler.h"
#include "src/cfg/cfg.h"
#include "src/code/checker.h"
#include "src/io/att_reader.h"
#include "src/io/att_writer.h"
#include "src/io/intel_reader.h"
#include "src/io/intel_writer.h"

using namespace std;
using namespace x64;

namespace {

inline int io_state() {
	static int i = ios_base::xalloc();
	return i;
}

inline int transform_state() {
	static int i = ios_base::xalloc();
	return i;
}

template <typename T>
inline IO get_io(T& ios) {
	return (IO) ios.iword(io_state());
}

template <typename T>
inline Transform get_transform(T& ios) {
	return (Transform) ios.iword(transform_state());
}

template <typename T>
ostream& generic_write(ostream& os, const T t) {
	if ( !Checker::check(t) )
		os.setstate(ios::failbit);

	switch ( get_io(os) ) {
		case ATT:
			AttWriter::write(os, t);
			break;
		case INTEL:
			IntelWriter::write(os, t);
			break;

		default:
			os.setstate(ios::failbit);
			break;
	}

	return os;
}

template <typename T>
ostream& extended_generic_write(ostream& os, const T t) {
	if ( !Checker::check(t) )
		os.setstate(ios::failbit);

	Assembler assm;
	switch ( get_io(os) ) {
		case ATT:
			AttWriter::write(os, t);
			break;
		case ELF:
			assm.start_elf(os);
			assm.assemble(t);
			assm.finish_elf(os);
			break;
		case HEX:
			assm.start_hex(os);
			assm.assemble(t);
			assm.finish_hex(os);
			break;
		case INTEL:
			IntelWriter::write(os, t);
			break;

		default:
			os.setstate(ios::failbit);
			break;
	}

	return os;
}

} // namespace

#if 0
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
#endif

ostream& operator<<(ostream& os, const Code& c) {
	return extended_generic_write(os, c);
}

ostream& operator<<(ostream& os, const Instruction& i) {
	return extended_generic_write(os, i);
}

ostream& operator<<(ostream& os, const Opcode o) {
	return generic_write(os, o);
}

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

ostream& operator<<(ostream& os, const M m) {
	return generic_write(os, m);
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
