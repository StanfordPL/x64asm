#include "src/code/stream.h"

using namespace std;
using namespace x64;

namespace {

inline int state_i() {
	static int i = ios_base::xalloc();
	return i;
}

inline int width_i() {
	static int i = ios_base::xalloc();
	return i;
}

template <typename T>
inline FormatVal get_format(T& ios) {
	return (FormatVal) ios.iword(state_i());
}

inline BitWidth get_width(ostream& os) {
	return (BitWidth) os.iword(width_i());
}

template <typename T>
inline istream& read(istream& is, T& t) {
	switch ( get_format(is) ) {
		case ATT: t.read_att(is); break;
		default:  is.setstate(ios::failbit); break;
	}
	return is;
}

template <typename T>
inline ostream& write(ostream& os, const T& t) {
	switch ( get_format(os) ) {
		case ATT: t.write_att(os);    break;
		default: os.setstate(ios::failbit); break;
	}
	return os;
}

template <typename T>
inline ostream& write_width(ostream& os, const T& t) {
	const auto w = get_width(os);
	switch ( get_format(os) ) {
		case ATT: t.write_att(os, w); break;
		default: os.setstate(ios::failbit); break;
	}
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

ostream& operator<<(ostream& os, const width& w) {
	os.iword(width_i()) = w;
	return os;
}

istream& operator>>(istream& is, Addr& a) {
	return read(is, a);
}

istream& operator>>(istream& is, Code& c) {
	return read(is, c);
}

istream& operator>>(istream& is, CondReg& c) {
	return read(is, c);
}

istream& operator>>(istream& is, GpReg& g) { 
	return read(is, g); 
}

istream& operator>>(istream& is, Imm& i) { 
	return read(is, i); 
}

istream& operator>>(istream& is, Instruction& i) { 
	return read(is, i); 
}

istream& operator>>(istream& is, Label& l) { 
	return read(is, l); 
}

istream& operator>>(istream& is, Opcode& o) { 
	return read(is, o); 
}

istream& operator>>(istream& is, Scale& s) { 
	return read(is, s); 
}

istream& operator>>(istream& is, SegReg& s) { 
	return read(is, s); 
}

istream& operator>>(istream& is, XmmReg& x) { 
	return read(is, x); 
}

ostream& operator<<(ostream& os, const Addr& a) {
	return write_width(os, a);
}

ostream& operator<<(ostream& os, const Code& c) {
	return write(os, c);
}

ostream& operator<<(ostream& os, const CondReg& c) {
	return write(os, c);
}

ostream& operator<<(ostream& os, const GpReg& g) {
	return write_width(os, g);
}

ostream& operator<<(ostream& os, const Imm& i) { 
	return write_width(os, i); 
}

ostream& operator<<(ostream& os, const Instruction& i) { 
	return write(os, i); 
}

ostream& operator<<(ostream& os, const Label& l) { 
	return write(os, l); 
}

ostream& operator<<(ostream& os, const Opcode& o) { 
	return write(os, o); 
}

ostream& operator<<(ostream& os, const Scale& s) { 
	return write(os, s); 
}

ostream& operator<<(ostream& os, const SegReg& s) { 
	return write(os, s); 
}

ostream& operator<<(ostream& os, const XmmReg& x) { 
	return write(os, x); 
}

