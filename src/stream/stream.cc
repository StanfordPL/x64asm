#include "src/stream/stream.h"

#include "src/assembler/assembler.h"
#include "src/cfg/cfg.h"
#include "src/cfg/remove_nop.h"
#include "src/cfg/remove_unreachable.h"
#include "src/io/dot_writer.h"
#include "src/io/elf_writer.h"
#include "src/io/hex_writer.h"

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
inline void check(ostream& os, const T& t) {
	if ( !t.check() )
		os.setstate(ios::failbit);
}

template <typename T>
ostream& generic_write(ostream& os, const T& t) {
	switch ( get_io(os) ) {
		case IO::ATT:
			t.write_att(os);
			break;
		case IO::INTEL:
			t.write_intel(os);
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
			c.read_att(is);
			break;
		case IO::INTEL:
			c.read_intel(is);
			break;

		default:
			is.setstate(ios::failbit);
			break;
	}

	switch ( get_transform(is) ) {
		case Transform::ALL:
			RemoveNop::remove(c);
			RemoveUnreachable::remove(c);
			break;
		case Transform::REMOVE_NOP:
			RemoveNop::remove(c);
			break;
		case Transform::REMOVE_UNREACHABLE:
			RemoveUnreachable::remove(c);
			break;

		default:
			is.setstate(ios::failbit);
			break;
	}

	return is;
}

ostream& operator<<(ostream& os, const Code& c) {
	check(os, c);

	Code code = c;
	switch ( get_transform(os) ) {
		case Transform::ALL:
			RemoveNop::remove(code);
			RemoveUnreachable::remove(code);
			break;
		case Transform::REMOVE_NOP:
			RemoveNop::remove(code);
			break;
		case Transform::REMOVE_UNREACHABLE:
			RemoveUnreachable::remove(code);
			break;

		default:
			os.setstate(ios::failbit);
			break;
	}

	switch ( get_io(os) ) {
		case IO::ATT:
			code.write_att(os);
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
			code.write_intel(os);
			break;

		default:
			os.setstate(ios::failbit);
			break;
	}

	return os;
}

ostream& operator<<(ostream& os, const Cr& c) {
	return generic_write(os, c);
}

ostream& operator<<(ostream& os, const Dr& d) {
	return generic_write(os, d);
}

ostream& operator<<(ostream& os, const Eflag& e) {
	return generic_write(os, e);
}

ostream& operator<<(ostream& os, const Imm& i) {
	return generic_write(os, i);
}

ostream& operator<<(ostream& os, const Instruction& i) {
	check(os, i);
	return generic_write(os, i);
}

ostream& operator<<(ostream& os, const Label& l) {
	return generic_write(os, l);
}

ostream& operator<<(ostream& os, const M& m) {
	check(os, m);
	return generic_write(os, m);
}

ostream& operator<<(ostream& os, const Mm& m) {
	return generic_write(os, m);
}

ostream& operator<<(ostream& os, const Moffs& m) {
	return generic_write(os, m);
}

ostream& operator<<(ostream& os, const OpSet& o) {
	return generic_write(os, o);
}

ostream& operator<<(ostream& os, const Rl& r) {
	return generic_write(os, r);
}

ostream& operator<<(ostream& os, const Rh& r) {
	return generic_write(os, r);
}

ostream& operator<<(ostream& os, const Rb& r) {
	return generic_write(os, r);
}

ostream& operator<<(ostream& os, const R16& r) {
	return generic_write(os, r);
}

ostream& operator<<(ostream& os, const R32& r) {
	return generic_write(os, r);
}

ostream& operator<<(ostream& os, const R64& r) {
	return generic_write(os, r);
}

ostream& operator<<(ostream& os, const Rel& r) {
	return generic_write(os, r);
}

ostream& operator<<(ostream& os, const Sreg& s) {
	return generic_write(os, s);
}

ostream& operator<<(ostream& os, const St& s) {
	return generic_write(os, s);
}

ostream& operator<<(ostream& os, const Xmm& x) {
	return generic_write(os, x);
}

ostream& operator<<(ostream& os, const Ymm& y) {
	return generic_write(os, y);
}
