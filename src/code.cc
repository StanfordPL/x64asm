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

#include "src/code.h"

#include "src/att.tab.h"

using namespace std;

namespace x64asm {

bool Code::check() const {
	for ( const auto& i : *this )
		if ( !i.check() )
			return false;
	return true;
}

void Code::read_att(istream& is) {
	attparse(is, *this);
}

void Code::read_intel(istream& is) {
	// TODO
}

void Code::write_att(ostream& os) const {
	for ( size_t i = 0, ie = size(); i < ie; ++i ) {
		(*this)[i].write_att(os);
		if ( i+1 != ie )
			os << endl;	
	}
}

void Code::write_intel(ostream& os) const {
	for ( size_t i = 0, ie = size(); i < ie; ++i ) {
		(*this)[i].write_intel(os);
		if ( i+1 != ie )
			os << endl;	
	}
}

} // namespace x64asm
