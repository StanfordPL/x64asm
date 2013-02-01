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

#include "src/op_set.h"

using namespace std;

namespace x64asm {

M::~M() {
	// Does nothing.
}

bool M::check() const {
	// Check seg
	if ( contains_seg() && !get_seg()->check() )
		return false;
	// Check base
	if ( contains_base() && !get_base()->check() )
		return false;
	// Check index
	if ( contains_index() && !get_index()->check() )
		return false;
	// Check disp
	if ( contains_disp() && !get_disp()->check() )
		return false;

	// Both base and index can't both be null
	if ( !contains_base() && !contains_index() ) 
		return false;
	// Index cannot be rsp/esp
	if ( contains_index() && get_index()->val_ == rsp.val_ )
		return false;

	return true;
}

void M::write_att(ostream& os) const {
	if ( contains_seg() ) {
		get_seg()->write_att(os);
		os << ":";
	}
	if ( contains_disp() )
		get_disp()->write_att(os);
	os << "(";
	if ( contains_base() ) {
		const auto b = get_base();
		if ( get_addr_or() )
			((R32*)b)->write_att(os);
		else
			((R64*)b)->write_att(os);
	}
	if ( contains_base() && contains_index() )
		os << ",";
	if ( contains_index() ) {
		const auto i = get_index();
		if ( get_addr_or() )
			((R32*)i)->write_att(os);
		else
			((R64*)i)->write_att(os);
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
		get_seg()->write_intel(os);
		os << ":";
	}
	os << "[";
	if ( contains_base() ) {
		const auto b = get_base();
		if ( get_addr_or() )
			((R32*)b)->write_intel(os);
		else
			((R64*)b)->write_intel(os);
	}
	if ( contains_base() && contains_index() )
		os << "+";
	if ( contains_index() ) {
		const auto i = get_index();
		if ( get_addr_or() )
			((R32*)i)->write_intel(os);
		else
			((R64*)i)->write_intel(os);
		os << "*";
		switch ( get_scale() ) {
			case Scale::TIMES_1: os << "1"; break;
			case Scale::TIMES_2: os << "2"; break;
			case Scale::TIMES_4: os << "4"; break;
			case Scale::TIMES_8: os << "8"; break;
			default: assert(false);
		}
	}
	if ( contains_disp() ) {
		os << "+";
		get_disp()->write_intel(os);
	}
	os << "]";
}

void M::insert_in(OpSet& os, bool promote) const {
	os += *this;
}

void M8::write_intel_width(ostream& os) const {
	os << "BYTE ";
}

OpType M8::type() const {
	return OpType::M_8;
}

void M16::write_intel_width(ostream& os) const {
	os << "WORD ";
}

OpType M16::type() const {
	return OpType::M_16;
}

void M32::write_intel_width(ostream& os) const {
	os << "DWORD ";
}

OpType M32::type() const {
	return OpType::M_32;
}

void M64::write_intel_width(ostream& os) const {
	os << "QWORD ";
}

OpType M64::type() const {
	return OpType::M_64;
}

void M128::write_intel_width(ostream& os) const {
	os << "XMMWORD ";
}

OpType M128::type() const {
	return OpType::M_128;
}

void M256::write_intel_width(ostream& os) const {
	os << "YMMWORD ";
}

OpType M256::type() const {
	return OpType::M_256;
}

void MPtr1616::write_intel_width(ostream& os) const {
	os << "";
}

OpType MPtr1616::type() const {
	return OpType::M_PTR_16_16;
}

void MPtr1632::write_intel_width(ostream& os) const {
	os << "";
}

OpType MPtr1632::type() const {
	return OpType::M_PTR_16_32;
}

void MPtr1664::write_intel_width(ostream& os) const {
	os << "";
}

OpType MPtr1664::type() const {
	return OpType::M_PTR_16_64;
}

void M16Int::write_intel_width(ostream& os) const {
	os << "WORD ";
}

OpType M16Int::type() const {
	return OpType::M_16_INT;
}

void M32Int::write_intel_width(ostream& os) const {
	os << "DWORD ";
}

OpType M32Int::type() const {
	return OpType::M_32_INT;
}

void M64Int::write_intel_width(ostream& os) const {
	os << "QWORD ";
}

OpType M64Int::type() const {
	return OpType::M_64_INT;
}

void M32Fp::write_intel_width(ostream& os) const {
	os << "DWORD ";
}

OpType M32Fp::type() const {
	return OpType::M_32_FP;
}

void M64Fp::write_intel_width(ostream& os) const {
	os << "QWORD ";
}

OpType M64Fp::type() const {
	return OpType::M_64_FP;
}

void M80Fp::write_intel_width(ostream& os) const {
	os << "TBYTE ";
}

OpType M80Fp::type() const {
	return OpType::M_80_FP;
}

void M80Bcd::write_intel_width(ostream& os) const {
	os << "TBYTE ";
}

OpType M80Bcd::type() const {
	return OpType::M_80_BCD;
}

void M2Byte::write_intel_width(ostream& os) const {
	os << "";
}

OpType M2Byte::type() const {
	return OpType::M_2_BYTE;
}

void M14Byte::write_intel_width(ostream& os) const {
	os << "";
}

OpType M14Byte::type() const {
	return OpType::M_14_BYTE;
}

void M28Byte::write_intel_width(ostream& os) const {
	os << "";
}

OpType M28Byte::type() const {
	return OpType::M_28_BYTE;
}

void M94Byte::write_intel_width(ostream& os) const {
	os << "";
}

OpType M94Byte::type() const {
	return OpType::M_94_BYTE;
}

void M108Byte::write_intel_width(ostream& os) const {
	os << "";
}

OpType M108Byte::type() const {
	return OpType::M_108_BYTE;
}

void M512Byte::write_intel_width(ostream& os) const {
	os << "";
}

OpType M512Byte::type() const {
	return OpType::M_512_BYTE;
}

} // namespace x64asm
