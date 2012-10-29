#ifndef X64_SRC_CODE_XMM_H
#define X64_SRC_CODE_XMM_H

#include <vector>

#include "src/code/operand.h"

namespace x64 {

/** An SSE register: xmm0, ..., xmm15.
*/
class Xmm {
	public:
		inline Xmm() 
				: x_(16) { 
		}

		inline Xmm(Operand x) 
				: x_(x) { 
		}

		inline operator Operand() const {
			return x_;
		}

		inline bool is_null() const {
			return x_ >= 16;	
		}

		typedef const std::vector<Xmm>::const_iterator iterator;

		static iterator begin() {
			return domain_.begin();
		}

		static iterator end() {
			return domain_.end();
		}

	private:
		Operand x_;

		static const std::vector<Xmm> domain_;
};

class Xmm0 : public Xmm {
	public:
		inline Xmm0() : Xmm() { }
		inline Xmm0(Operand o) : Xmm(o) { }

		inline bool is_null() const { return (Operand)*this != 0; }
};

extern const Xmm0 xmm0;
extern const Xmm xmm1;
extern const Xmm xmm2;
extern const Xmm xmm3;
extern const Xmm xmm4;
extern const Xmm xmm5;
extern const Xmm xmm6;
extern const Xmm xmm7;
extern const Xmm xmm8;
extern const Xmm xmm9;
extern const Xmm xmm10;
extern const Xmm xmm11;
extern const Xmm xmm12;
extern const Xmm xmm13;
extern const Xmm xmm14;
extern const Xmm xmm15;
extern const Xmm xmm_null;

} // namespace x64

#endif
