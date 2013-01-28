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

#include "src/sreg.h"

#include <cassert>

using namespace std;

namespace x64asm {

OpType Sreg::type() const {
	return OpType::SREG;
}

bool Sreg::check() const {
	return val() < 6;
}

void Sreg::write_att(ostream& os) const {
	os << "%";
	write_intel(os);
}

void Sreg::write_intel(ostream& os) const {
	switch ( val() ) {
		case 0: os << "es"; break;
		case 1: os << "cs"; break;
		case 2: os << "ss"; break;
		case 3: os << "ds"; break;
		case 4: os << "fs"; break;
		case 5: os << "gs"; break;

		default: assert(false);
	}
}

OpType Fs::type() const {
	return OpType::FS;
}

bool Fs::check() const {
	return val() == 4;
}

OpType Gs::type() const {
	return OpType::GS;
}

bool Gs::check() const {
	return val() == 5;
}

} // namespace x64asm
