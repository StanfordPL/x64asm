#ifndef X64_SRC_CODE_EFLAG_H
#define X64_SRC_CODE_EFLAG_H

#include <vector>

#include "src/code/operand.h"

namespace x64 {

/** An EFLAGS register bit. */
class Eflag {
	public:
		inline Eflag() 
				: e_{0} {
		}

		inline Eflag(Operand o)
				: e_{o} {
		}

		inline operator Operand() const {
			return e_;
		}

	private:
		Operand e_;
};

extern const Eflag cf;
extern const Eflag pf;
extern const Eflag af;
extern const Eflag zf;
extern const Eflag sf;
extern const Eflag tf;
extern const Eflag if_;
extern const Eflag df;
extern const Eflag of;
extern const Eflag iopl;
extern const Eflag nt;
extern const Eflag rf;
extern const Eflag vm;
extern const Eflag ac;
extern const Eflag vif;
extern const Eflag vip;
extern const Eflag id;

typedef std::vector<Eflag> Eflags;
extern const Eflags eflags;

} // namespace x64

#endif
