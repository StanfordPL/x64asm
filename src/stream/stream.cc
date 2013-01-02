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
inline void check(ostream& os, const T t) {
	if ( !Checker::check(t) )
		os.setstate(ios::failbit);
}

template <typename T>
ostream& generic_write(ostream& os, const T t) {
	switch ( get_io(os) ) {
		case IO::ATT:
			AttWriter::write(os, t);
			break;
		case IO::INTEL:
			IntelWriter::write(os, t);
			break;

		default:
			os.setstate(ios::failbit);
			break;
	}

	return os;
}

template <typename T>
ostream& extended_generic_write(ostream& os, const T& t) {
	Assembler assm;
	switch ( get_io(os) ) {
		case IO::ATT:
			AttWriter::write(os, t);
			break;
		case IO::ELF:
			assm.start_elf(os);
			assm.assemble(t);
			assm.finish_elf(os);
			break;
		case IO::HEX:
			assm.start_hex(os);
			assm.assemble(t);
			assm.finish_hex(os);
			break;
		case IO::INTEL:
			IntelWriter::write(os, t);
			break;

		default:
			os.setstate(ios::failbit);
			break;
	}

	return os;
}

} // namespace

istream& operator>>(istream& is, const set_io& m) {
	is.iword(io_state()) = m;
	return is;
}

istream& operator>>(istream& is, const set_transform& m) {
	is.iword(transform_state()) |= m;
	return is;
}

istream& operator>>(istream& is, const unset_transform& m) {
	is.iword(transform_state()) &= ~m;
	return is;
}

ostream& operator<<(ostream& os, const set_io& m) {
	os.iword(io_state()) = m;
	return os;
}

ostream& operator<<(ostream& os, const set_transform& m) {
	os.iword(transform_state()) |= m;
	return os;
}

ostream& operator<<(ostream& os, const unset_transform& m) {
	os.iword(transform_state()) &= ~m;
	return os;
}

istream& operator>>(istream& is, Code& c) {
	// TODO...
#if 0
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
	return is;
}

ostream& operator<<(ostream& os, const Code& c) {
	check(os, c);
	return extended_generic_write(os, c);
	// TODO; need to switch on transformation state before returning.
}

ostream& operator<<(ostream& os, const Cr0234 c) {
	check(os, c);
	return generic_write(os, c);
}

ostream& operator<<(ostream& os, const Cr8 c) {
	check(os, c);
	return generic_write(os, c);
}

ostream& operator<<(ostream& os, const Dr d) {
	check(os, d);
	return generic_write(os, d);
}

ostream& operator<<(ostream& os, const Eflag e) {
	check(os, e);
	return generic_write(os, e);
}

ostream& operator<<(ostream& os, const Imm8 i) {
	check(os, i);
	return generic_write(os, i);
}

ostream& operator<<(ostream& os, const Imm16 i) {
	check(os, i);
	return generic_write(os, i);
}

ostream& operator<<(ostream& os, const Imm32 i) {
	check(os, i);
	return generic_write(os, i);
}

ostream& operator<<(ostream& os, const Imm64 i) {
	check(os, i);
	return generic_write(os, i);
}

ostream& operator<<(ostream& os, const Instruction& i) {
	check(os, i);
	return extended_generic_write(os, i);
}

ostream& operator<<(ostream& os, const Label l) {
	check(os, l);
	return generic_write(os, l);
}

ostream& operator<<(ostream& os, const M m) {
	check(os, m);
	return generic_write(os, m);
}

ostream& operator<<(ostream& os, const Mm m) {
	check(os, m);
	return generic_write(os, m);
}

ostream& operator<<(ostream& os, const Moffs m) {
	check(os, m);
	return generic_write(os, m);
}

ostream& operator<<(ostream& os, const Opcode o) {
	return generic_write(os, o);
}

ostream& operator<<(ostream& os, const NoRexR8 r) {
	check(os, r);
	return generic_write(os, r);
}

ostream& operator<<(ostream& os, const RexR8 r) {
	check(os, r);
	return generic_write(os, r);
}

ostream& operator<<(ostream& os, const Rl r) {
	check(os, r);
	return generic_write(os, r);
}

ostream& operator<<(ostream& os, const Rh r) {
	check(os, r);
	return generic_write(os, r);
}

ostream& operator<<(ostream& os, const Rb r) {
	check(os, r);
	return generic_write(os, r);
}

ostream& operator<<(ostream& os, const R16 r) {
	check(os, r);
	return generic_write(os, r);
}

ostream& operator<<(ostream& os, const R32 r) {
	check(os, r);
	return generic_write(os, r);
}

ostream& operator<<(ostream& os, const R64 r) {
	check(os, r);
	return generic_write(os, r);
}

ostream& operator<<(ostream& os, const Rel8 r) {
	check(os, r);
	return generic_write(os, r);
}

ostream& operator<<(ostream& os, const Rel32 r) {
	check(os, r);
	return generic_write(os, r);
}

ostream& operator<<(ostream& os, const Sreg s) {
	check(os, s);
	return generic_write(os, s);
}

ostream& operator<<(ostream& os, const St s) {
	check(os, s);
	return generic_write(os, s);
}

ostream& operator<<(ostream& os, const Xmm x) {
	check(os, x);
	return generic_write(os, x);
}

ostream& operator<<(ostream& os, const Ymm y) {
	check(os, y);
	return generic_write(os, y);
}
