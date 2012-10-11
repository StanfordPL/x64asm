#ifndef X64_SRC_CODE_REG_SET_H
#define X64_SRC_CODE_REG_SET_H

#include <cassert>
#include <list>

#include "src/code/cond_reg.h"
#include "src/code/gp_reg.h"
#include "src/code/operand.h"
#include "src/code/xmm_reg.h"

namespace x64 {

/** An efficient datastructure for tracking register sets.
*/
class RegSet {
	private:
		enum Mask {
			M_NULL   = 0x0000000000000000,
			M_LOW    = 0x0000000000000001,
			M_HIGH   = 0x0000000000010000,
			M_WORD   = 0x0000000000010001,
			M_DOUBLE = 0x0000000100010001,
			M_QUAD   = 0x0001000100010001,
			M_XMM    = 0x0000000000000001,
			M_CR     = 0x0000000100000000
		};

	public:
		typedef std::list<GpReg>::const_iterator gp_reg_iterator;
		typedef std::list<XmmReg>::const_iterator xmm_reg_iterator;
		typedef std::list<CondReg>::const_iterator cond_reg_iterator;

		inline RegSet() {
			clear();
		}

		inline RegSet& clear() {
			gp_mask_ = 0;
			xmm_cr_mask_ = 0;
			return *this;
		}

		inline RegSet& set_gp(GpReg r, BitWidth w) {
			assert(!r.is_null());
			assert(w < FIXED);
			static Mask m[5] { M_LOW, M_HIGH, M_WORD, M_DOUBLE, M_QUAD };
			gp_mask_ |= (m[w] << r);
			return *this;
		}

		inline RegSet& set_xmm(XmmReg r) {
			assert(!r.is_null());
			xmm_cr_mask_ |= (M_XMM << r);	
			return *this;
		}

		inline RegSet& set_cond(CondReg r) {
			assert(!r.is_null());
			xmm_cr_mask_ |= (M_CR << r);
			return *this;
		}

		inline bool is_set_gp(GpReg r, BitWidth w) const {
			assert(!r.is_null());
			assert(w < FIXED);
			static Mask m[5] { M_LOW, M_HIGH, M_WORD, M_DOUBLE, M_QUAD };
			return gp_mask_ & (m[w] << r);
		}

		BitWidth get_widest_set(GpReg r) const;

		inline bool is_set_xmm(XmmReg r) const {
			assert(!r.is_null());
			return xmm_cr_mask_ & (M_XMM << r);
		}

		inline bool is_set_cond(CondReg r) const {
			assert(!r.is_null());
			return xmm_cr_mask_ & (M_CR << r);
		}

		inline gp_reg_iterator gp_begin(BitWidth w) const {
			switch ( w ) {
				case LOW:    build<GpReg, M_LOW, 16>(gp_regs_, gp_mask_);    break;
				case HIGH:   build<GpReg, M_HIGH, 16>(gp_regs_, gp_mask_);   break;
				case WORD:   build<GpReg, M_WORD, 16>(gp_regs_, gp_mask_);   break;
				case DOUBLE: build<GpReg, M_DOUBLE, 16>(gp_regs_, gp_mask_); break;
				case QUAD:   build<GpReg, M_QUAD, 16>(gp_regs_, gp_mask_);   break;
				default:     assert(false); 
			}
			return gp_regs_.begin();
		}

		inline gp_reg_iterator gp_end(BitWidth w) const {
			return gp_regs_.end();
		}

		inline xmm_reg_iterator xmm_begin() const {
			build<XmmReg, M_XMM, 16>(xmm_regs_, xmm_cr_mask_);
			return xmm_regs_.begin();
		}

		inline xmm_reg_iterator xmm_end() const {
			return xmm_regs_.end();
		}

		inline cond_reg_iterator cond_begin() const {
			build<CondReg, M_CR, 6>(cond_regs_, xmm_cr_mask_);
			return cond_regs_.begin();
		}

		inline cond_reg_iterator cond_end() const {
			return cond_regs_.end();
		}

		inline RegSet operator~() const {
			RegSet rs;
			rs.gp_mask_ = ~gp_mask_;
			rs.xmm_cr_mask_ = ~xmm_cr_mask_;
			return rs;
		}

		inline RegSet operator&(const RegSet& rhs) const {
			RegSet res = *this;
			res &= rhs;
		 	return res;
		}

		inline RegSet operator|(const RegSet& rhs) const {
			RegSet res = *this;
			res |= rhs;
			return res;
		}

		inline RegSet operator^(const RegSet& rhs) const {
			RegSet res = *this;
			res ^= rhs;
			return res;
		}	

		inline RegSet operator-(const RegSet& rhs) const {
			RegSet res = *this;
			res -= rhs;
			return res;
		}

		inline RegSet& operator&=(const RegSet& rhs) {
			gp_mask_ &= rhs.gp_mask_;
			xmm_cr_mask_ &= rhs.xmm_cr_mask_;
			return *this;
		}

		inline RegSet& operator|=(const RegSet& rhs) {
			gp_mask_ |= rhs.gp_mask_;
			xmm_cr_mask_ |= rhs.xmm_cr_mask_;
			return *this;
		}

		inline RegSet& operator^=(const RegSet& rhs) {
			gp_mask_ ^= rhs.gp_mask_;
			xmm_cr_mask_ ^= rhs.xmm_cr_mask_;
			return *this;
		}

		inline RegSet& operator-=(const RegSet& rhs) {
			gp_mask_ &= ~rhs.gp_mask_;
			xmm_cr_mask_ &= ~rhs.xmm_cr_mask_;
			return *this;
		}

		inline bool operator==(const RegSet& rhs) const {
			return gp_mask_ == rhs.gp_mask_ && xmm_cr_mask_ == rhs.xmm_cr_mask_;
		}

		inline bool operator!=(const RegSet& rhs) const {
			return gp_mask_ != rhs.gp_mask_ && xmm_cr_mask_ != rhs.xmm_cr_mask_;
		}

	private:
		uint64_t gp_mask_;
		uint64_t xmm_cr_mask_;

		static std::list<GpReg> gp_regs_;
		static std::list<XmmReg> xmm_regs_;
		static std::list<CondReg> cond_regs_;
		
		template <typename T, Mask M, int Max>
		static inline void build(std::list<T>& v, uint64_t m) {
			v.clear();
			for ( int i = 0; i < Max; ++i ) {
				if ( m & M )
					v.push_back((T) i);
				m >>= 1;
			}
		}
};

} // namespace x64

#endif
