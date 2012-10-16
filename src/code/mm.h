#ifndef X64_SRC_CODE_MM_REG_H
#define X64_SRC_CODE_MM_REG_H

#include <vector>

#include "src/code/operand.h"

namespace x64 {

/** An MMX register: mm0, ..., mm7.
*/
class Mm {
	public:
		inline Mm() 
				: m_(8) { 
		}

		inline Mm(Operand m) 
				: m_(m) { 
		}

		inline operator Operand() const {
			return m_;
		}

		inline bool is_null() const {
			return m_ >= 8;	
		}

		typedef const std::vector<Mm>::const_iterator iterator;

		static iterator begin() {
			return domain_.begin();
		}

		static iterator end() {
			return domain_.end();
		}

	private:
		Operand m_;

		static const std::vector<Mm> domain_;
};

extern const Mm mm0;
extern const Mm mm1;
extern const Mm mm2;
extern const Mm mm3;
extern const Mm mm4;
extern const Mm mm5;
extern const Mm mm6;
extern const Mm mm7;
extern const Mm mm_null;

} // namespace x64

#endif

