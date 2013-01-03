#include "src/stream/stream.h"

#include "src/assembler/assembler.h"
#include "src/cfg/cfg.h"
#include "src/cfg/remove_nop.h"
#include "src/cfg/remove_unreachable.h"
#include "src/code/checker.h"
#include "src/io/att_reader.h"
#include "src/io/att_writer.h"
#include "src/io/dot_writer.h"
#include "src/io/elf_writer.h"
#include "src/io/hex_writer.h"
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
	switch ( get_io(is) ) {
		case IO::ATT:
			AttReader::read(is, c);
			break;
		case IO::INTEL:
			IntelReader::read(is, c);
			break;

		default:
			is.setstate(ios::failbit);
			break;
	}

	switch ( get_transform(is) ) {
		case Transform::ALL:
			c = RemoveNop::run(c);
			c = RemoveUnreachable::run(c);
			break;
		case Transform::REMOVE_NOP:
			c = RemoveNop::run(c);
			break;
		case Transform::REMOVE_UNREACHABLE:
			c = RemoveUnreachable::run(c);
			break;

		default:
			is.setstate(ios::failbit);
			break;
	}

	return is;
}

ostream& operator<<(ostream& os, const Code& c) {
	check(os, c);

	Code code;
	switch ( get_transform(os) ) {
		case Transform::ALL:
			code = RemoveNop::run(code);
			code = RemoveUnreachable::run(code);
			break;
		case Transform::REMOVE_NOP:
			code = RemoveNop::run(code);
			break;
		case Transform::REMOVE_UNREACHABLE:
			code = RemoveUnreachable::run(code);
			break;

		default:
			os.setstate(ios::failbit);
			break;
	}

	switch ( get_io(os) ) {
		case IO::ATT:
			AttWriter::write(os, code);
			break;
		case IO::DOT:
			DotWriter::write(os, code);
			break;
		case IO::ELF:
			ElfWriter::write(os, code);
			break;
		case IO::HEX:
			HexWriter::write(os, code);
			break;
		case IO::INTEL:
			IntelWriter::write(os, code);
			break;

		default:
			os.setstate(ios::failbit);
			break;
	}

	return os;
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
	return generic_write(os, i);
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
