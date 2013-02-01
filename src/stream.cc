/*
Copyright 2103 eric schkufza

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

#include "src/stream.h"

#include "src/assembler.h"

using namespace std;
using namespace x64asm;

namespace {

inline int syntax_state() {
	static int i = ios_base::xalloc();
	return i;
}

template <typename T>
inline Syntax get_syntax(T& ios) {
	return (Syntax)ios.iword(syntax_state());
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

istream& operator>>(istream& is, const Syntax s) {
	is.iword(syntax_state()) = (long)s;
	return is;
}

ostream& operator<<(ostream& os, const Syntax s) {
	os.iword(syntax_state()) = (long)s;
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

ostream& operator<<(ostream& os, const Code& c) {
	check(os, c);
	return write(os, c);
}

ostream& operator<<(ostream& os, const Instruction& i) {
	check(os, i);
	return write(os, i);
}

ostream& operator<<(ostream& os, const Operand& o) {
	check(os, o);
	return write(os, o);
}
