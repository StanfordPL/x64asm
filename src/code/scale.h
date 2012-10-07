#ifndef X64_SRC_CODE_SCALE_H
#define X64_SRC_CODE_SCALE_H 
#include <cassert>
#include <iostream>
#include <vector>

#include "src/code/operand.h"

namespace x64 {

/** Scale values.
*/
enum ScaleVal { 
	TIMES_1 = 0, 
	TIMES_2, 
	TIMES_4, 
	TIMES_8,

	NUM_SCALE_VALS,
	SCALE_VAL_NULL = NUM_SCALE_VALS
};

/** A scaling constant for use in forming addresses.
*/
class Scale {
	public:

		typedef std::vector<Scale>::const_iterator iterator;

		static iterator begin() {
			return range_.begin();
		}

		static iterator end() {
			return range_.end();
		}

		inline Scale() 
				: s_(TIMES_1) { 
		}

		inline Scale(ScaleVal s) 
				: s_(s) { 
			assert(is_valid());
		}

		inline operator Operand() const {
			return s_;
		}

		inline bool is_null() const {
			return s_ == SCALE_VAL_NULL;
		}

		inline bool is_valid() const { 
			return s_ <= SCALE_VAL_NULL; 
		}

		inline void read_att(std::istream& is) {
			is.setstate(std::ios::failbit);
		}

		inline void write_att(std::ostream& os) const {
			switch ( s_ ) {
				case TIMES_1: os << "1"; break;
				case TIMES_2: os << "2"; break;
				case TIMES_4: os << "4"; break;
				case TIMES_8: os << "8"; break;
				default: os.setstate(std::ios::failbit);
			}
		}

	private:
		Operand s_;

		static std::vector<Scale> range_;
};

} // namespace x64

#endif
