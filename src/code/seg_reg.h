#ifndef X64_SRC_CODE_SEG_REG_H
#define X64_SRC_CODE_SEG_REG_H

#include <cassert>
#include <iostream>

#include "src/code/operand.h"

namespace x64 {

/** A Segmentation register: CS, DS, SS, ES, FS, GS
*/
class SegReg {
	public:
		inline SegReg()
				: s_(6) {
		}

		inline SegReg(Operand s)
				: s_(s) {
			assert(is_valid());
		}

		inline operator Operand() const {
			return s_;
		}

		inline bool is_null() const {
			return s_ == 6;
		}

		inline bool is_valid() const {
			return s_ <= 6;
		}

		inline void read_att(std::istream& is) {
			is.setstate(std::ios::failbit);
		}

		inline void write_att(std::ostream& os) const {
			switch ( s_ ) {
				case 0: os << "es"; break;
				case 1: os << "cs"; break;
				case 2: os << "ss"; break;
				case 3: os << "ds"; break;
				case 4: os << "fs"; break;
				case 5: os << "gs"; break;
				default: assert(false); break;
			}
		}

	private:
		Operand s_;
};

extern const SegReg es;
extern const SegReg cs;
extern const SegReg ss;
extern const SegReg ds;
extern const SegReg fs;
extern const SegReg gs;
extern const SegReg seg_null;

} // namespace x64

#endif
