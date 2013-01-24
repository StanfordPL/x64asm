#include "src/stream.h"

#include "src/assembler.h"

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
inline void check(ostream& os, const T& t) {
	if ( !t.check() )
		os.setstate(ios::failbit);
}

template <typename T>
ostream& write_txt(ostream& os, const T& t) {
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

ostream& operator<<(ostream& os, const syntax& s) {
	os.iword(syntax_state()) = s;
	return os;
}

ostream& operator<<(ostream& os, const format& f) {
	os.iword(format_state()) = f;
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

	return is;
}

ostream& operator<<(ostream& os, const Cfg& c) {
	return write_txt(os, c);
}

ostream& operator<<(ostream& os, const Code& c) {
	check(os, c);
	switch ( get_format(os) ) {
		case Format::DEBUG:
			switch ( get_syntax(os) ) {
				case Syntax::ATT:
					Assembler().write_att(os, c);
					break;
				case Syntax::INTEL:
					Assembler().write_intel(os, c);
					break;

				default:
					os.setstate(ios::failbit);
					break;
			}
			break;
		case Format::DOT:
			write_txt(os, Cfg{c});
			break;
		case Format::ELF:
			Assembler().write_elf(os, c);
			break;
		case Format::HEX:
			Assembler().write_hex(os, c);
			break;
		case Format::TXT:
			write_txt(os, c);
			break;

		default:
			os.setstate(ios::failbit);
			break;
	}

	return os;
}

ostream& operator<<(ostream& os, const Instruction& i) {
	check(os, i);
	return write_txt(os, i);
}

ostream& operator<<(ostream& os, const Operand& o) {
	check(os, o);
	return write_txt(os, o);
}

ostream& operator<<(ostream& os, const OpSet& o) {
	return write_txt(os, o);
}
