#ifndef X64_SRC_CODE_REG_SET_H
#define X64_SRC_CODE_REG_SET_H

#include <cassert>
#include <vector>

#include "src/code/cond_reg.h"
#include "src/code/r.h"
#include "src/code/operand.h"
#include "src/code/xmm.h"

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
		typedef std::vector<RH>::const_iterator  rh_iterator;
		typedef std::vector<R8>::const_iterator  r8_iterator;
		typedef std::vector<R16>::const_iterator r16_iterator;
		typedef std::vector<R32>::const_iterator r32_iterator;
		typedef std::vector<R64>::const_iterator r64_iterator;
		typedef std::vector<Xmm>::const_iterator xmm_iterator;
		typedef std::vector<CondReg>::const_iterator cond_reg_iterator;

		inline RegSet() {
			clear();
		}

		inline RegSet& clear() {
			r_mask_ = 0;
			xmm_cr_mask_ = 0;
			return *this;
		}

		inline RegSet& set(RH r) {
			assert(!r.is_null());
			r_mask_ |= (M_HIGH << r);
			return *this;
		}

		inline RegSet& set(R8 r) {
			assert(!r.is_null());
			r_mask_ |= (M_LOW << r);
			return *this;
		}

		inline RegSet& set(R16 r) {
			assert(!r.is_null());
			r_mask_ |= (M_WORD << r);
			return *this;
		}

		inline RegSet& set(R32 r) {
			assert(!r.is_null());
			r_mask_ |= (M_DOUBLE << r);
			return *this;
		}

		inline RegSet& set(R64 r) {
			assert(!r.is_null());
			r_mask_ |= (M_QUAD << r);
			return *this;
		}

		inline RegSet& set(Xmm r) {
			assert(!r.is_null());
			xmm_cr_mask_ |= (M_XMM << r);	
			return *this;
		}

		inline RegSet& set(CondReg r) {
			assert(!r.is_null());
			xmm_cr_mask_ |= (M_CR << r);
			return *this;
		}

		inline bool is_set(RH r) const {
			assert(!r.is_null());
			return r_mask_ & (M_HIGH << r);
		}

		inline bool is_set(R8 r) const {
			assert(!r.is_null());
			return r_mask_ & (M_LOW << r);
		}

		inline bool is_set(R16 r) const {
			assert(!r.is_null());
			return r_mask_ & (M_WORD << r);
		}

		inline bool is_set(R32 r) const {
			assert(!r.is_null());
			return r_mask_ & (M_DOUBLE << r);
		}

		inline bool is_set(R64 r) const {
			assert(!r.is_null());
			return r_mask_ & (M_QUAD << r);
		}

		BitWidth get_widest_set(R r) const;

		inline bool is_set(Xmm r) const {
			assert(!r.is_null());
			return xmm_cr_mask_ & (M_XMM << r);
		}

		inline bool is_set(CondReg r) const {
			assert(!r.is_null());
			return xmm_cr_mask_ & (M_CR << r);
		}

		inline rh_iterator rh_begin() const {
			build<RH, M_HIGH, 4>(rh_, r_mask_);
			return rh_.begin();
		}
			
		inline rh_iterator rh_end() const {
			return rh_.end();
		}

		inline r8_iterator r8_begin() const {
			build<R8, M_LOW, 16>(r8_, r_mask_);
			return r8_.begin();
		}
			
		inline r8_iterator r8_end() const {
			return r8_.end();
		}

		inline r16_iterator r16_begin() const {
			build<R16, M_WORD, 16>(r16_, r_mask_);
			return r16_.begin();
		}
			
		inline r16_iterator r16_end() const {
			return r16_.end();
		}

		inline r32_iterator r32_begin() const {
			build<R32, M_DOUBLE, 16>(r32_, r_mask_);
			return r32_.begin();
		}
			
		inline r32_iterator r32_end() const {
			return r32_.end();
		}

		inline r64_iterator r64_begin() const {
			build<R64, M_QUAD, 16>(r64_, r_mask_);
			return r64_.begin();
		}
			
		inline r64_iterator r64_end() const {
			return r64_.end();
		}

		inline xmm_iterator xmm_begin() const {
			build<Xmm, M_XMM, 16>(xmm_, xmm_cr_mask_);
			return xmm_.begin();
		}

		inline xmm_iterator xmm_end() const {
			return xmm_.end();
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
			rs.r_mask_ = ~r_mask_;
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
			r_mask_ &= rhs.r_mask_;
			xmm_cr_mask_ &= rhs.xmm_cr_mask_;
			return *this;
		}

		inline RegSet& operator|=(const RegSet& rhs) {
			r_mask_ |= rhs.r_mask_;
			xmm_cr_mask_ |= rhs.xmm_cr_mask_;
			return *this;
		}

		inline RegSet& operator^=(const RegSet& rhs) {
			r_mask_ ^= rhs.r_mask_;
			xmm_cr_mask_ ^= rhs.xmm_cr_mask_;
			return *this;
		}

		inline RegSet& operator-=(const RegSet& rhs) {
			r_mask_ &= ~rhs.r_mask_;
			xmm_cr_mask_ &= ~rhs.xmm_cr_mask_;
			return *this;
		}

		inline bool operator==(const RegSet& rhs) const {
			return r_mask_ == rhs.r_mask_ && xmm_cr_mask_ == rhs.xmm_cr_mask_;
		}

		inline bool operator!=(const RegSet& rhs) const {
			return r_mask_ != rhs.r_mask_ || xmm_cr_mask_ != rhs.xmm_cr_mask_;
		}

	private:
		uint64_t r_mask_;
		uint64_t xmm_cr_mask_;

		static std::vector<RH> rh_;
		static std::vector<R8> r8_;
		static std::vector<R16> r16_;
		static std::vector<R32> r32_;
		static std::vector<R64> r64_;
		static std::vector<Xmm> xmm_;
		static std::vector<CondReg> cond_regs_;
		
		template <typename T, Mask M, int Max>
		static inline void build(std::vector<T>& v, uint64_t m) {
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
