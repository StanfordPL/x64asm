#include "src/code/gp_reg.h"

using namespace std;

namespace x64 {

const GpReg rax = 0;
const GpReg rcx = 1;
const GpReg rdx = 2;
const GpReg rbx = 3;
const GpReg rsp = 4;
const GpReg rbp = 5;
const GpReg rsi = 6;
const GpReg rdi = 7;
const GpReg r8  = 8;
const GpReg r9  = 9;
const GpReg r10 = 10;
const GpReg r11 = 11;
const GpReg r12 = 12;
const GpReg r13 = 13;
const GpReg r14 = 14;
const GpReg r15 = 15;
const GpReg gp_null = 16;

void GpReg::write_att(ostream& os, BitWidth w) const {
	switch ( g_ ) {
		#define ABCD(R,Q,D,W,H,L)                \
			case R: switch ( w ) {                 \
								case QUAD:   os << Q; break; \
				  			case DOUBLE: os << D; break; \
				  			case WORD:   os << W; break; \
								case HIGH:   os << H; break; \
								case LOW:    os << L; break; \
								default:                     \
									os.setstate(ios::failbit); \
							}                              \
							break;
		ABCD(0, "%rax", "%eax", "%ax", "%ah", "%al")
		ABCD(1, "%rcx", "%ecx", "%cx", "%ch", "%cl")
		ABCD(2, "%rdx", "%edx", "%dx", "%dh", "%dl")
		ABCD(3, "%rbx", "%ebx", "%bx", "%bh", "%bl")
		#undef ABCD

		#define OTH(R,Q,D,W,L)                   \
			case R: switch ( w ) {                 \
								case QUAD:   os << Q; break; \
				  			case DOUBLE: os << D; break; \
				  			case WORD:   os << W; break; \
								case LOW:    os << L; break; \
								default:                     \
									os.setstate(ios::failbit); \
							}                              \
							break;
		OTH(4,  "%rsp", "%esp",  "%sp",   "%spl")
		OTH(5,  "%rbp", "%ebp",  "%bp",   "%bpl")
		OTH(6,  "%rsi", "%esi",  "%si",   "%sil")
		OTH(7,  "%rdi", "%edi",  "%di",   "%dil")
		OTH(8,  "%r8",  "%r8d",  "%r8w",  "%r8b")
		OTH(9,  "%r9",  "%r9d",  "%r9w",  "%r9b")
		OTH(10, "%r10", "%r10d", "%r10w", "%r10b")
		OTH(11, "%r11", "%r11d", "%r11w", "%r11b")
		OTH(12, "%r12", "%r12d", "%r12w", "%r12b")
		OTH(13, "%r13", "%r13d", "%r13w", "%r13b")
		OTH(14, "%r14", "%r14d", "%r14w", "%r14b")
		OTH(15, "%r15", "%r15d", "%r15w", "%r15b")
		#undef OTH

		default:
			os.setstate(ios::failbit);
	}
}

} // namespace x64
