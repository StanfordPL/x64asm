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

#include "src/modifier.h"

using namespace std;

namespace x64asm {

Modifier::~Modifier() {
	// Does nothing.
}

void Modifier::write_att(ostream& os) const {
	// Does nothing.
}

void Modifier::write_intel(ostream& os) const {
	// Does nothing.
}

bool Pref66::check() const {
	return val_ == 0;
}

OpType Pref66::type() const {
	return OpType::PREF_66;
}

bool PrefRexW::check() const {
	return val_ == 0;
}

OpType PrefRexW::type() const {
	return OpType::PREF_REX_W;
}

bool Far::check() const {
	return val_ == 0;
}

OpType Far::type() const {
	return OpType::FAR;
}

} // namespace x64asm
