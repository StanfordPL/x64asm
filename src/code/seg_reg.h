#ifndef X64_SRC_CODE_SEG_REG_H
#define X64_SRC_CODE_SEG_REG_H

#include <vector>

#include "src/code/operand.h"

namespace x64 {

/** A Segmentation register: cs, ds, ss, es, fs, gs
*/
class Sreg {
	public:
		inline Sreg()
				: s_(6) {
		}

		inline Sreg(Operand s)
				: s_(s) {
		}

		inline operator Operand() const {
			return s_;
		}

		inline bool is_null() const {
			return s_ >= 6;
		}

		typedef const std::vector<Sreg>::const_iterator iterator;

		static iterator begin() {
			return domain_.begin();
		}

		static iterator end() {
			return domain_.end();
		}

	private:
		Operand s_;

		static const std::vector<Sreg> domain_;
};

extern const Sreg es;
extern const Sreg cs;
extern const Sreg ss;
extern const Sreg ds;
extern const Sreg fs;
extern const Sreg gs;
extern const Sreg sreg_null;

} // namespace x64

#endif
