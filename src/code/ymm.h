#ifndef X64_SRC_CODE_YMM_H
#define X64_SRC_CODE_YMM_H

#include <vector>

#include "src/code/operand.h"

namespace x64 {

class Ymm {
	public:
		inline Ymm() 
				: x_(16) { 
		}

		inline Ymm(Operand x) 
				: x_(x) { 
		}

		inline operator Operand() const {
			return x_;
		}

		inline bool is_null() const {
			return x_ >= 16;	
		}

		typedef const std::vector<Ymm>::const_iterator iterator;

		static iterator begin() {
			return domain_.begin();
		}

		static iterator end() {
			return domain_.end();
		}

	private:
		Operand x_;

		static const std::vector<Ymm> domain_;
};

extern const Ymm ymm0;
extern const Ymm ymm1;
extern const Ymm ymm2;
extern const Ymm ymm3;
extern const Ymm ymm4;
extern const Ymm ymm5;
extern const Ymm ymm6;
extern const Ymm ymm7;
extern const Ymm ymm8;
extern const Ymm ymm9;
extern const Ymm ymm10;
extern const Ymm ymm11;
extern const Ymm ymm12;
extern const Ymm ymm13;
extern const Ymm ymm14;
extern const Ymm ymm15;
extern const Ymm ymm_null;

} // namespace x64

#endif

