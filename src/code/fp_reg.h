#ifndef X64_SRC_CODE_FP_REG_H
#define X64_SRC_CODE_FP_REG_H

#include <vector>

#include "src/code/operand.h"

namespace x64 {

class St {
	public:
		inline St() 
				: f_(8) { 
		}

		inline St(Operand f) 
				: f_(f) { 
		}

		inline operator Operand() const {
			return f_;
		}

		inline bool is_null() const {
			return f_ >= 8;	
		}

		inline bool is_top() const {
			return f_ == 0;
		}

		typedef const std::vector<St>::const_iterator iterator;

		static iterator begin() {
			return domain_.begin();
		}

		static iterator end() {
			return domain_.end();
		}

	private:
		Operand f_;

		static const std::vector<St> domain_;
};

class St0 : public St {
	public:
		inline St0() : St() { }
		inline St0(Operand o) : St(o) { }
};

extern const St0 st0;
extern const St st1;
extern const St st2;
extern const St st3;
extern const St st4;
extern const St st5;
extern const St st6;
extern const St st7;

} // namespace x64

#endif


