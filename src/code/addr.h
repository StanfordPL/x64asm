#ifndef X64_SRC_CODE_ADDR_H
#define X64_SRC_CODE_ADDR_H

#include <cassert>
#include <iostream>

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
		inline Addr() {
			set_all(seg_null,
					    gp_null,
							gp_null,
							TIMES_1,
							0);
		}

		inline Addr(Operand o)
				: a_(o) {
		}

		inline Addr(GpReg b) {
			set_all(seg_null,
					    b,
							gp_null,
							TIMES_1,
							0);
		}

		inline Addr(GpReg b, Imm d) {
			set_all(seg_null,
					    b,
							gp_null,
							TIMES_1,
							d);
		}

		inline Addr(GpReg b, GpReg i) {
			set_all(seg_null, b, i, TIMES_1, 0);
		}

		inline Addr(GpReg b, GpReg i, ScaleVal s) {
			set_all(seg_null, b, i, s, 0);
		}

		inline Addr(GpReg b, GpReg i, Imm d) {
			set_all(seg_null, b, i, TIMES_1, d);
		}

		inline Addr(GpReg b, GpReg i, ScaleVal s, Imm d) {
			set_all(seg_null, b, i, s, d);
		}

		inline operator Operand() const {
			return a_;
		}

		inline bool is_null() const {
			return get_seg().is_null() && 
				     get_base().is_null() &&
				     get_index().is_null();
		}

		inline bool is_valid() const {
			const auto seg = get_seg();
			if ( seg != fs && seg != gs && seg != seg_null )
				return false;
			return get_base().is_valid() && 
						 get_index().is_valid() &&
				     get_scale().is_valid() && 
						 get_disp().is_valid();
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
			return (ScaleVal) ((a_ >> 32)  & 0x7);
		}

		inline Imm get_disp() const {
			return (Operand) (a_ & 0xffffffff);
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

		inline void set_all(SegReg s, GpReg b, GpReg i, Scale sc, Imm d) {
			set_seg(s);
			set_base(b);
			set_index(i);
			set_scale(sc);
			set_disp(d);
		}

		inline void read_att(std::istream& is) {
			is.setstate(std::ios::failbit);
		}

		void write_att(std::ostream& os, BitWidth w = QUAD) const;

	private:
		// seg    base   index  scale  disp
		// [47:45][44:40][39:35][34:32][31:0]
		Operand a_;
};

} // namespace x64

#endif
