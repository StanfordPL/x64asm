#ifndef X64_SRC_CODE_SREG_H
#define X64_SRC_CODE_SREG_H

#include <vector>

#include "src/code/operand.h"

namespace x64 {

/** A segment register. The segment register bit assignments are ES = 0, 
	  CS = 1, SS = 2, DS = 3, FS = 4, and GS = 5.
*/
class Sreg {
	public:
		inline Sreg()
				: s_{0} {
		}

		inline Sreg(Operand s)
				: s_{s} {
		}

		inline operator Operand() const {
			return s_;
		}

	private:
		Operand s_;
};

/** The segment register FS. */
struct Fs : public Sreg {
	inline Fs() : Sreg{4} { }
};

/** The segment register GS. */
struct Gs : public Sreg {
	inline Gs() : Sreg{5} { }
};

extern const Sreg es;
extern const Sreg cs;
extern const Sreg ss;
extern const Sreg ds;
extern const Fs fs;
extern const Gs gs;

typedef std::vector<Sreg> Sregs;
extern const Sregs sregs;

} // namespace x64

#endif
