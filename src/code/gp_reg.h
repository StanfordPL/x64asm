#ifndef X64_SRC_CODE_GP_REG_H
#define X64_SRC_CODE_GP_REG_H

#include "src/code/operand.h"

namespace x64 {

/** A general purpose register: 
		rax, rbx, rcx, rdx, rbp, rsp, rdi, rsi, r8, ..., r15.
*/
class GpReg {
	public:

		inline GpReg() 
				: g_(16) { 
		}
		
		inline GpReg(Operand g) 
				: g_(g) { 
		}

		inline operator Operand() const {
			return g_;
		}

		inline bool is_null() const {
			return g_ >= 16;
		}

		inline bool is_abcd() const {
			return g_ < 4;
		}

		inline bool is_extended() const {
			return g_ > 7 && g_ < 16;
		}

		inline bool is_pointer() const {
			return g_ == 4 || g_ == 5;
		}

		inline bool is_index() const {
			return g_ == 6 || g_ == 7;
		}

	private:
		Operand g_;
};

extern const GpReg rax;
extern const GpReg rcx;
extern const GpReg rdx;
extern const GpReg rbx;
extern const GpReg rsp;
extern const GpReg rbp;
extern const GpReg rsi;
extern const GpReg rdi;
extern const GpReg r8;
extern const GpReg r9;
extern const GpReg r10;
extern const GpReg r11;
extern const GpReg r12;
extern const GpReg r13;
extern const GpReg r14;
extern const GpReg r15;
extern const GpReg gp_null;

} // namespace x64

#endif
