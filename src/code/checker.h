#ifndef X64_SRC_CODE_CHECKER_H
#define X64_SRC_CODE_CHECKER_H

#include "src/code/code.h"
#include "src/code/instruction.h"
#include "src/operands/constants.h"
#include "src/operands/cr.h"
#include "src/operands/dr.h"
#include "src/operands/eflag.h"
#include "src/operands/imm.h"
#include "src/operands/label.h"
#include "src/operands/m.h"
#include "src/operands/mm.h"
#include "src/operands/modifier.h"
#include "src/operands/moffs.h"
#include "src/operands/operand.h"
#include "src/operands/r.h"
#include "src/operands/rel.h"
#include "src/operands/sreg.h"
#include "src/operands/st.h"
#include "src/operands/xmm.h"
#include "src/operands/ymm.h"

namespace x64 {

class Checker {
	public:
		static inline bool check(const Cr c) {
			return check((Cr0234)c) || check((Cr8)c);
		}

		static inline bool check(const Cr0234 c) {
			if ( c.val_ == 0 ) return true;
			if ( c.val_ == 2 ) return true;
			if ( c.val_ == 3 ) return true;
			if ( c.val_ == 4 ) return true;
			return false;
		}

		static inline bool check(const Cr8 c) {
			return c.val_ == 8;
		}

		static inline bool check(const Dr d) {
			return d.val_ < 8;
		}

		static inline bool check(const Eflag e) {
			if ( e.val_ > 21 ) return false;
			if ( e.val_ == 1 ) return false;
			if ( e.val_ == 3 ) return false;
			if ( e.val_ == 5 ) return false;
			return true;
		}

		static inline bool check(const Imm i) {
			// The check for Imm64 subsumed every other Imm.
			return check((Imm64)i);
		}

		static inline bool check(const Imm8 i) {
			const auto val = (int64_t) i.val_;
			return val >= -128 && val <= 127;
		}

		static inline bool check(const Imm16 i) {
			const auto val = (int64_t) i.val_;
			return val >= -32768 && val <= 32767;
		}

		static inline bool check(const Imm32 i) {
			const auto val = (int64_t) i.val_;
			return val >= -2147483648 && val <= 2147483647;
		}

		static inline bool check(const Imm64 i) {
			return true;
		}

		static inline bool check(const Zero z) {
			return z.val_ == 0;
		}

		static inline bool check(const One o) {
			return o.val_ == 1;
		}

		static inline bool check(const Three t) {
			return t.val_ == 3;
		}

		static inline bool check(const Label l) {
			return true;
		}

		static inline bool check(const M m) {
			// Both base and index can't both be null
			if ( m.null_base() && m.null_index() ) 
				return false;
			// Check non-null bases
			if ( !m.null_base() && m.get_base().val_ >= 16 )
				return false;
			// Check non-null indices
			if ( !m.null_index() && m.get_index().val_ >= 16 )
				return false;
			// Index cannot be rsp/esp
			if ( !m.null_index() && m.get_index().val_ == rsp.val_ )
				return false;
			return true;
		}

		static inline bool check(const Mm m) {
			return m.val_ < 8;
		}

		static inline bool check(const Modifier m) {
			// All three modifiers share the same correctness check.
			return check((Pref66)m);
		}

		static inline bool check(const Pref66 p) {
			return p.val_ == 0;
		}

		static inline bool check(const PrefRexW p) {
			return p.val_ == 0;
		}

		static inline bool check(const Far f) {
			return f.val_ == 0;
		}

		static inline bool check(const Moffs m) {
			return true;
		}

		static inline bool check(const R r) {
			return r.val_ < 16;
		}

		static inline bool check(const NoRexR8 r) {
			return ((Rl*)&r)->val_ < 16;
		}

		static inline bool check(const RexR8 r) {
			return ((Rl*)&r)->val_ < 8;
		}

		static inline bool check(const Rl r) {
			return r.val_ < 4;
		}

		static inline bool check(const Rh r) {
			return r.val_ >= 4 && r.val_ < 8;
		}

		static inline bool check(const Rb r) {
			return r.val_ >= 8 && r.val_ < 16;
		}

		static inline bool check(const Al r) {
			return r.val_ == 0;
		}

		static inline bool check(const Cl r) {
			return r.val_ == 1;
		}

		static inline bool check(const R16 r) {
			return r.val_ < 16;
		}

		static inline bool check(const Ax r) {
			return r.val_ == 0;
		}

		static inline bool check(const Dx r) {
			return r.val_ == 2;
		}

		static inline bool check(const R32 r) {
			return r.val_ < 16; 
		}

		static inline bool check(const Eax r) {
			return r.val_ == 0;
		}

		static inline bool check(const R64 r) {
			return r.val_ < 16;
		}

		static inline bool check(const Rax r) {
			return r.val_ < 16;
		}

		static inline bool check(const Rel r) {
			// The check for Rel32 subsumes the check for Rel8
			return check((Rel32)r);
		}

		static inline bool check(const Rel8 r) {
			const auto val = (int64_t) r.val_;
			return val >= -128 && val <= 127;
		}

		static inline bool check(const Rel32 r) {
			const auto val = (int64_t) r.val_;
			return val >= -2147483648 && val <= 2147483647;
		}

		static inline bool check(const Sreg s) {
			return s.val_ < 6;
		}

		static inline bool check(const Fs f) {
			return f.val_ == 4;
		}

		static inline bool check(const Gs g) {
			return g.val_ == 5;
		}

		static inline bool check(const St s) {
			return s.val_ < 8;
		}

		static inline bool check(const St0 s) {
			return s.val_ == 0;
		}

		static inline bool check(const Xmm x) {
			return x.val_ < 16;
		}

		static inline bool check(const Xmm0 x) {
			return x.val_ == 0;
		}

		static inline bool check(const Ymm y) {
			return y.val_ == 0;
		}

		static bool check(const Instruction& i);

		static inline bool check(const Code& c) {
			for ( const auto& instr : c )
				if ( !check(instr) )
					return false;
			return true;
		}
};

} // namespace x64

#endif
