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

#include "src/st.h"

#include <cassert>

using namespace std;

namespace x64asm {

OpType St::type() const {
	return OpType::ST;
}

bool St::check() const {
	return val() < 8;
}

void St::write_att(ostream& os) const {
	os << "%";
	write_intel(os);
}

void St::write_intel(ostream& os) const {
	assert(check());
	if ( val() == 0 )
		os << "st";	
	else
		os << "st(" << dec << val() << ")";
}

OpType St0::type() const {
	return OpType::ST_0;
}

bool St0::check() const {
	return val() == 0;
}

} // namespace x64asm
