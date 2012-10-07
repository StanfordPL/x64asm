#ifndef X64_SRC_CODE_IMM_H
#define X64_SRC_CODE_IMM_H

#include <cassert>
#include <iostream>
#include <sstream>

#include "src/code/operand.h"

namespace x64 {

/** An unsigned 64-bit immediate.
*/
class Imm {
	public:
		
		inline Imm() 
				: i_(0) { 
		}

		inline Imm(Operand o) 
				: i_(o) {
		}
		
		inline operator Operand() const { 
			return i_;
		}

		inline bool is_null() const {
			return false;
		}

		inline bool is_valid() const {
			return true;
		}

		inline void read_att(std::istream& is) {
			is.setstate(std::ios::failbit);
		}

		inline void write_att(std::ostream& os, BitWidth w = QUAD) const {
			os << std::hex << std::showbase << "$";
			switch ( w ) {
				case LOW:    os << (i_ & 0x00000000000000ff); break;
				case WORD:   os << (i_ & 0x000000000000ffff); break;
				case DOUBLE: os << (i_ & 0x00000000ffffffff); break;
				case QUAD:   os << i_; break;
				default:		 os.setstate(std::ios::failbit);
			}
		}

	private:
		Operand i_;
};

} // namespace x64

#endif
