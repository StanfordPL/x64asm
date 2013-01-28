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

#include "src/eflag.h"

#include "src/op_set.h"

#include <cassert>

using namespace std;

namespace x64asm {

OpType Eflag::type() const {
	return OpType::EFLAG;
}

bool Eflag::check() const {
	return val() < 22 && val() != 1 && val() != 3 && val() != 5 && val() != 15;
}

void Eflag::insert_in(OpSet& os, bool promote) const {
	os += *this;
}

void Eflag::write_att(ostream& os) const {
	os << "%";
	write_intel(os);
}

void Eflag::write_intel(ostream& os) const {
	switch ( val() ) {
		case 0: os << "cf"; break;
		case 2: os << "pf"; break;
		case 4: os << "af"; break;
		case 6: os << "zf"; break;
		case 7: os << "sf"; break;
		case 8: os << "tf"; break;
		case 9: os << "if"; break;
		case 10: os << "dr"; break;
		case 11: os << "of"; break;
		case 12: os << "iopl[0]"; break;
		case 13: os << "iopl[1]"; break;
		case 14: os << "nt"; break;
		case 16: os << "rf"; break;
		case 17: os << "vm"; break;
		case 18: os << "ac"; break;
		case 19: os << "vif"; break;
		case 20: os << "vip"; break;
		case 21: os << "id"; break;

		default: assert(false);
	}
}

} // namespace x64asm
