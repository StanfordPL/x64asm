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

#include "src/rel.h"

using namespace std;

namespace x64asm {

Rel::~Rel() {
	// Does nothing.
}

OpType Rel::type() const {
	return OpType::REL;
}

void Rel::write_att(ostream& os) const {
	os << hex << showbase << val_;
}

void Rel::write_intel(ostream& os) const {
}

OpType Rel8::type() const {
	return OpType::REL_8;
}

bool Rel8::check() const {
	return (int64_t)val_ >= -128 && (int64_t)val_ < 128;
}

OpType Rel32::type() const {
	return OpType::REL_32;
}

bool Rel32::check() const {
	return (int64_t)val_ >= -2147483648 && (int64_t)val_ < 2147483648;
}

} // namespace x64asm
