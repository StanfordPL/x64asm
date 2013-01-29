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

#include "src/mxcsr.h"

#include "src/op_set.h"

#include <cassert>

using namespace std;

namespace x64asm {

OpType Mxcsr::type() const {
	return OpType::MXCSR;
}

bool Mxcsr::check() const {
	return val() < 16 && val() != 13;
}

void Mxcsr::insert_in(OpSet& os, bool promote) const {
	os += *this;
}

void Mxcsr::write_att(ostream& os) const {
	os << "%";
	write_intel(os);
}

void Mxcsr::write_intel(ostream& os) const {
	switch ( val() ) {
		case 0: os << "ie"; break;
		case 1: os << "de"; break;
		case 2: os << "ze"; break;
		case 3: os << "oe"; break;
		case 4: os << "ue"; break;
		case 5: os << "pe"; break;
		case 6: os << "daz"; break;
		case 7: os << "im"; break;
		case 8: os << "dm"; break;
		case 9: os << "zm"; break;
		case 10: os << "om"; break;
		case 11: os << "um"; break;
		case 12: os << "pm"; break;
		case 14: os << "rc"; break;
		case 15: os << "fz"; break;

		default: assert(false);
	}
}

} // namespace x64asm


