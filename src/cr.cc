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

#include "src/cr.h"

#include <cassert>

using namespace std;

namespace x64asm {

OpType Cr::type() const {
	return OpType::CR;
}

bool Cr::check() const {
	return val() == 0 || val() == 2 || val() == 3 || val() == 4 || val() == 8;
}

void Cr::write_att(ostream& os) const {
	os << "%";
	write_intel(os);
}

void Cr::write_intel(ostream& os) const {
	assert(check());
	os << "cr" << dec << val();
}

OpType Cr0234::type() const {
	return OpType::CR_0234;
}

bool Cr0234::check() const {
	return val() == 0 || val() == 2 || val() == 3 || val() == 4;
}

OpType Cr8::type() const {
	return OpType::CR_8;
}

bool Cr8::check() const {
	return val() == 8;
}

} // namespace x64asm
