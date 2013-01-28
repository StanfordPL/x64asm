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

#include "src/xmm.h"

#include "src/op_set.h"

#include <cassert>

using namespace std;

namespace x64asm {

OpType Xmm::type() const {
	return OpType::XMM;
}

bool Xmm::check() const {
	return val() < 16;
}

void Xmm::insert_in(OpSet& os, bool promote) const {
	if ( promote )
		os += parent();
	else
		os += *this;
}

void Xmm::write_att(ostream& os) const {
	os << "%";
	write_intel(os);
}

void Xmm::write_intel(ostream& os) const {
	assert(check());
	os << "xmm" << dec << val();
}

Ymm Xmm::parent() const {
	return Ymm{val()};
}

OpType Xmm0::type() const {
	return OpType::XMM_0;
}

bool Xmm0::check() const {
	return val() == 0;
}

} // namespace x64asm
