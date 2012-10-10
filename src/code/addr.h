#ifndef X64_SRC_CODE_ADDR_H
#define X64_SRC_CODE_ADDR_H

#include <cassert>

#include "src/code/gp_reg.h"
#include "src/code/imm.h"
#include "src/code/operand.h"
#include "src/code/seg_reg.h"
#include "src/code/scale.h"

namespace x64 {

/** An address in memory.
*/
class Addr {
	public:
		inline Addr(Operand o)
				: a_(o) {
		}

		inline Addr(GpReg b, bool size_or = false) {
			set_all(seg_null, b, gp_null, times_1, 0, size_or);
		}

		inline Addr(GpReg b, Imm d, bool size_or = false) {
			set_all(seg_null, b, gp_null, times_1, d, size_or);
		}

		inline Addr(GpReg b, GpReg i, bool size_or = false) {
			set_all(seg_null, b, i, times_1, 0, size_or);
		}

		inline Addr(GpReg b, GpReg i, Scale s, bool size_or = false) {
			set_all(seg_null, b, i, s, 0, size_or);
		}

		inline Addr(GpReg b, GpReg i, Imm d, bool size_or = false) {
			set_all(seg_null, b, i, times_1, d, size_or);
		}

		inline Addr(GpReg b, GpReg i, Scale s, Imm d, bool size_or = false) {
			set_all(seg_null, b, i, s, d, size_or);
		}

		inline operator Operand() const {
			return a_;
		}

		inline SegReg get_seg() const {
			return (SegReg) ((a_ >> 45) & 0x7);
		}

		inline GpReg get_base() const {
			return (GpReg) ((a_ >> 40) & 0x1f);
		}

		inline GpReg get_index() const {
			return (GpReg) ((a_ >> 35) & 0x1f);
		}

		inline Scale get_scale() const {
			return (Scale) ((a_ >> 32)  & 0x7);
		}

		inline Imm get_disp() const {
			return (Operand) (a_ & 0xffffffff);
		}

		inline bool get_size_or() const {
			return (a_ >> 48) & 0x1;
		}

		inline void set_seg(SegReg s) {
			a_ = (a_ & ~((Operand) 0x7 << 45)) | (s << 45);
		}

		inline void set_base(GpReg b) {
			a_ = (a_ & ~((Operand) 0x1f << 40)) | (b << 40); 
		}

		inline void set_index(GpReg i) {
			a_ = (a_ & ~((Operand) 0x1f << 35)) | (i << 35); 
		}

		inline void set_scale(Scale s) {
			assert(!s.is_null());
			a_ = (a_ & ~((Operand) 0x7 << 32)) | (s << 32); 
		}

		inline void set_disp(Imm d) {
			assert(((d >> 32) == 0) || ((d >> 32) == 0xffffffff));
			a_ = (a_ & ~((Operand) 0xffffffff)) | (d & 0xffffffff);
		}

		inline void set_size_or(bool size_or) {
			if ( size_or )
				a_ |= ((Operand) 0x1 << 48);
			else
				a_ &= ~((Operand) 0x1 << 48);
		}

	private:
		// or  seg    base   index  scale  disp
		// [48][47:45][44:40][39:35][34:32][31:0]
		Operand a_;

		inline void set_all(SegReg s, GpReg b, GpReg i, Scale sc, Imm d, bool so) {
			set_seg(s);
			set_base(b);
			set_index(i);
			set_scale(sc);
			set_disp(d);
			set_size_or(so);
		}
};

} // namespace x64

#endif
