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

#include "src/m.h"

#include "src/reg_set.h"

using namespace std;

namespace x64asm {

bool M::check() const {
	// Check seg
	if ( contains_seg() && !get_seg().check() )
		return false;
	// Check base
	if ( contains_base() && !get_base().check() )
		return false;
	// Check index
	if ( contains_index() && !get_index().check() )
		return false;
	// Check scale
	switch ( get_scale() ) {
		case Scale::TIMES_1:
		case Scale::TIMES_2:
		case Scale::TIMES_4:
		case Scale::TIMES_8: break;
		default:      return false;
	}
	// Check disp
	if ( !get_disp().check() )
		return false;

	// Index cannot be rsp/esp
	if ( contains_index() && get_index().val_ == esp.val_ )
		return false;

	// Check for absence of base/index for RIP+offset form
	if ( rip_offset() && (contains_base() || contains_index()) )
		return false;

	return true;
}

void M::write_att(ostream& os) const {
	if ( contains_seg() ) {
		get_seg().write_att(os);
		os << ":";
	}
	if ( get_disp().val_ != 0 )
		get_disp().write_intel(os);

	if ( !contains_base() && !contains_index() && !rip_offset() )
		return;

	os << "(";
	if ( rip_offset() ) 
		os << "%rip";
	if ( contains_base() ) {
		const auto b = get_base();
		if ( get_addr_or() )
			b.write_att(os);
		else
			b.parent().write_att(os);
	}
	if ( contains_base() && contains_index() )
		os << ",";
	if ( contains_index() ) {
		const auto i = get_index();
		if ( get_addr_or() )
			i.write_att(os);
		else
			i.parent().write_att(os);
		os << ",";
		switch ( get_scale() ) {
			case Scale::TIMES_1: os << "1"; break;
			case Scale::TIMES_2: os << "2"; break;
			case Scale::TIMES_4: os << "4"; break;
			case Scale::TIMES_8: os << "8"; break;
			default: assert(false);
		}
	}
	os << ")";
}

void M::write_intel(ostream& os) const {
	write_intel_width(os);
	os << "PTR ";
	if ( contains_seg() ) {
		get_seg().write_intel(os);
		os << ":";
	}

	if ( !contains_base() && !contains_index() && !rip_offset()) {
		get_disp().write_intel(os);
		return;
	}

	os << "[";
	if ( rip_offset() ) 
		os << "rip";
	if ( contains_base() ) {
		const auto b = get_base();
		if ( get_addr_or() )
			b.write_intel(os);
		else
			b.parent().write_intel(os);
	}
	if ( contains_base() && contains_index() )
		os << "+";
	if ( contains_index() ) {
		const auto i = get_index();
		if ( get_addr_or() )
			i.write_intel(os);
		else
			i.parent().write_intel(os);
		os << "*";
		switch ( get_scale() ) {
			case Scale::TIMES_1: os << "1"; break;
			case Scale::TIMES_2: os << "2"; break;
			case Scale::TIMES_4: os << "4"; break;
			case Scale::TIMES_8: os << "8"; break;
			default: assert(false);
		}
	}
	if ( get_disp().val_ != 0 ) {
		os << "+";
		get_disp().write_intel(os);
	}
	os << "]";
}

void M::write_intel_width(ostream& os) const {
	assert(false);
}

void M::insert_in(RegSet& os, bool promote) const {
	os += *this;
}

void M8::write_intel_width(ostream& os) const {
	os << "BYTE ";
}

void M16::write_intel_width(ostream& os) const {
	os << "WORD ";
}

void M32::write_intel_width(ostream& os) const {
	os << "DWORD ";
}

void M64::write_intel_width(ostream& os) const {
	os << "QWORD ";
}

void M128::write_intel_width(ostream& os) const {
	os << "XMMWORD ";
}

void M256::write_intel_width(ostream& os) const {
	os << "YMMWORD ";
}

void M16Int::write_intel_width(ostream& os) const {
	os << "WORD ";
}

void M32Int::write_intel_width(ostream& os) const {
	os << "DWORD ";
}

void M64Int::write_intel_width(ostream& os) const {
	os << "QWORD ";
}

void M32Fp::write_intel_width(ostream& os) const {
	os << "DWORD ";
}

void M64Fp::write_intel_width(ostream& os) const {
	os << "QWORD ";
}

void M80Fp::write_intel_width(ostream& os) const {
	os << "TBYTE ";
}

void M80Bcd::write_intel_width(ostream& os) const {
	os << "TBYTE ";
}

void M2Byte::write_intel_width(ostream& os) const {
	os << "";
}

void M28Byte::write_intel_width(ostream& os) const {
	os << "";
}

void M108Byte::write_intel_width(ostream& os) const {
	os << "";
}

void M512Byte::write_intel_width(ostream& os) const {
	os << "";
}

void FarPtr1616::write_intel_width(ostream& os) const {
	os << "WORD ";
}

void FarPtr1632::write_intel_width(ostream& os) const {
	os << "DWORD ";
}

void FarPtr1664::write_intel_width(ostream& os) const {
	os << "QWORD ";
}

} // namespace x64asm
