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

#include "src/ymm.h"

#include "src/reg_set.h"

#include <cassert>

using namespace std;

namespace x64asm {

constexpr bool Ymm::check() {
	return val_ < 16;
}

void Ymm::write_att(ostream& os) const {
	os << "%";
	write_intel(os);
}

void Ymm::write_intel(ostream& os) const {
	assert(check());
	os << "ymm" << dec << val_;
}

constexpr OpType Ymm::type() {
	return OpType::YMM;
}

void Ymm::insert_in(RegSet& os, bool promote) const {
	os += *this;
}

} // namespace x64asm

