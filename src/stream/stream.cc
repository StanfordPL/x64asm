#include "src/stream/stream.h"

#include "src/assembler/assembler.h"
#include "src/cfg/cfg.h"
#include "src/cfg/remove_nop.h"
#include "src/cfg/remove_unreachable.h"

using namespace std;
using namespace x64;

namespace {

inline int syntax_state() {
	static int i = ios_base::xalloc();
	return i;
}

inline int format_state() {
	static int i = ios_base::xalloc();
	return i;
}

inline int transform_state() {
	static int i = ios_base::xalloc();
	return i;
}

template <typename T>
inline Syntax get_syntax(T& ios) {
	return (Syntax)ios.iword(syntax_state());
}

template <typename T>
inline Format get_format(T& ios) {
	return (Format)ios.iword(format_state());
}

template <typename T>
inline Transform get_transform(T& ios) {
	return (Transform)ios.iword(transform_state());
}

template <typename T>
inline void check(ostream& os, const T& t) {
	if ( !t.check() )
		os.setstate(ios::failbit);
}

template <typename T>
ostream& write(ostream& os, const T& t) {
	switch ( get_syntax(os) ) {
		case Syntax::ATT:
			t.write_att(os);
			break;
		case Syntax::INTEL:
			t.write_intel(os);
			break;

		default:
			os.setstate(ios::failbit);
			break;
	}

	return os;
}

} // namespace

istream& operator>>(istream& is, const syntax& s) {
	is.iword(syntax_state()) = s;
	return is;
}

istream& operator>>(istream& is, const format& f) {
	is.iword(format_state()) = f;
	return is;
}

istream& operator>>(istream& is, const transform& t) {
	if ( t == Transform::NONE )
		is.iword(transform_state()) = (long)Transform::NONE;
	else
		is.iword(transform_state()) |= t;
	return is;
}

ostream& operator<<(ostream& os, const syntax& s) {
	os.iword(syntax_state()) = s;
	return os;
}

ostream& operator<<(ostream& os, const format& f) {
	os.iword(format_state()) = f;
	return os;
}

ostream& operator<<(ostream& os, const transform& t) {
	if ( t == Transform::NONE )
		os.iword(transform_state()) = (long)Transform::NONE;
	else
		os.iword(transform_state()) |= t;
	return os;
}

istream& operator>>(istream& is, Code& c) {
	switch ( get_syntax(is) ) {
		case Syntax::ATT:
			c.read_att(is);
			break;
		case Syntax::INTEL:
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
		case Transform::NONE:
			break;
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

	switch ( get_format(os) ) {
		case Format::TXT:
			write(os, c);
			break;
		case Format::ELF:
			break;
		case Format::HEX:
			break;
		case Format::DOT:
			break;

		default:
			os.setstate(ios::failbit);
			break;
	}

	return os;
}

ostream& operator<<(ostream& os, const Operand& o) {
	check(os, o);
	return write(os, o);
}

ostream& operator<<(ostream& os, const Instruction& i) {
	check(os, i);
	return write(os, i);
}

ostream& operator<<(ostream& os, const OpSet& o) {
	return write(os, o);
}
