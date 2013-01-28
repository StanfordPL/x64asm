#include "src/m.h"

#include "src/op_set.h"

using namespace std;

namespace x64asm {

OpType M::type() const {
	return OpType::M;
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
	if ( contains_index() && get_index()->val() == rsp.val() )
		return false;

	return true;
}

void M::insert_in(OpSet& os, bool promote) const {
	os += *this;
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
}

OpType M8::type() const {
	return OpType::M_8;
}

OpType M16::type() const {
	return OpType::M_16;
}

OpType M32::type() const {
	return OpType::M_32;
}

OpType M64::type() const {
	return OpType::M_64;
}

OpType M128::type() const {
	return OpType::M_128;
}

OpType M256::type() const {
	return OpType::M_256;
}

OpType MPair1664::type() const {
	return OpType::M_PAIR_16_64;
}

OpType MPtr1616::type() const {
	return OpType::M_PTR_16_16;
}

OpType MPtr1632::type() const {
	return OpType::M_PTR_16_32;
}

OpType MPtr1664::type() const {
	return OpType::M_PTR_16_64;
}

OpType M16Int::type() const {
	return OpType::M_16_INT;
}

OpType M32Int::type() const {
	return OpType::M_32_INT;
}

OpType M64Int::type() const {
	return OpType::M_64_INT;
}

OpType M32Fp::type() const {
	return OpType::M_32_FP;
}

OpType M64Fp::type() const {
	return OpType::M_64_FP;
}

OpType M80Fp::type() const {
	return OpType::M_80_FP;
}

OpType M80Bcd::type() const {
	return OpType::M_80_BCD;
}

OpType M2Byte::type() const {
	return OpType::M_2_BYTE;
}

OpType M14Byte::type() const {
	return OpType::M_14_BYTE;
}

OpType M28Byte::type() const {
	return OpType::M_28_BYTE;
}

OpType M94Byte::type() const {
	return OpType::M_94_BYTE;
}

OpType M108Byte::type() const {
	return OpType::M_108_BYTE;
}

OpType M512Byte::type() const {
	return OpType::M_512_BYTE;
}

} // namespace x64asm
