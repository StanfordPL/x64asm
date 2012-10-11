#ifndef X64_SRC_CODE_SEG_REG_H
#define X64_SRC_CODE_SEG_REG_H

#include "src/code/operand.h"

namespace x64 {

/** A Segmentation register: cs, ds, ss, es, fs, gs
*/
class SegReg {
	public:
		inline SegReg()
				: s_(6) {
		}

		inline SegReg(Operand s)
				: s_(s) {
		}

		inline operator Operand() const {
			return s_;
		}

		inline bool is_null() const {
			return s_ >= 6;
		}

	private:
		Operand s_;
};

extern const SegReg es;
extern const SegReg cs;
extern const SegReg ss;
extern const SegReg ds;
extern const SegReg fs;
extern const SegReg gs;
extern const SegReg seg_null;

} // namespace x64

#endif
