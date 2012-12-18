#ifndef X64_SRC_CODE_CONSTANTS_H
#define X64_SRC_CODE_CONSTANTS_H

#include <vector>

#include "src/code/cr.h"
#include "src/code/dr.h"
#include "src/code/eflag.h"
#include "src/code/imm.h"
#include "src/code/mm.h"
#include "src/code/modifier.h"

namespace x64 {

struct Constants {
	// Condition Registers (cr.h)
	static inline Cr0234 cr0() { return Cr0234{0}; }
	static inline Cr0234 cr2() { return Cr0234{2}; }
	static inline Cr0234 cr3() { return Cr0234{3}; }
	static inline Cr0234 cr4() { return Cr0234{4}; }
	static inline Cr8 cr8() { return Cr8{}; }

	// Debug Registers (dr.h)
	static inline Dr dr0() { return Dr{0}; }
	static inline Dr dr1() { return Dr{1}; }
	static inline Dr dr2() { return Dr{2}; }
	static inline Dr dr3() { return Dr{3}; }
	static inline Dr dr4() { return Dr{4}; }
	static inline Dr dr5() { return Dr{5}; }
	static inline Dr dr6() { return Dr{6}; }
	static inline Dr dr7() { return Dr{7}; }

	// EFlags (eflag.h)
	static inline Eflag cf() { return Eflag{0}; }
	static inline Eflag pf() { return Eflag{2}; }
	static inline Eflag af() { return Eflag{4}; }
	static inline Eflag zf() { return Eflag{6}; }
	static inline Eflag sf() { return Eflag{7}; }
	static inline Eflag tf() { return Eflag{8}; }
	static inline Eflag if_() { return Eflag{9}; }
	static inline Eflag df() { return Eflag{10}; }
	static inline Eflag of() { return Eflag{11}; }
	static inline Eflag iopl0() { return Eflag{12}; }
	static inline Eflag iopl1() { return Eflag{13}; }
	static inline Eflag nt() { return Eflag{14}; }
	static inline Eflag rf() { return Eflag{16}; }
	static inline Eflag vm() { return Eflag{17}; }
	static inline Eflag ac() { return Eflag{18}; }
	static inline Eflag vif() { return Eflag{19}; }
	static inline Eflag vip() { return Eflag{20}; }
	static inline Eflag id() { return Eflag{21}; }

	// Immediates (imm.h)
	static inline Zero zero() { return Zero(); }
	static inline One one() { return One(); }
	static inline Three three() { return Three(); }

	// MMX Registers (mm.h)
	static inline Mm mm0() { return Mm{0}; }
	static inline Mm mm1() { return Mm{1}; }
	static inline Mm mm2() { return Mm{2}; }
	static inline Mm mm3() { return Mm{3}; }
	static inline Mm mm4() { return Mm{4}; }
	static inline Mm mm5() { return Mm{5}; }
	static inline Mm mm6() { return Mm{6}; }
	static inline Mm mm7() { return Mm{7}; }

	// Modifiers (modifier.h)
	static inline Pref66 pref_66() { return Pref66(); }
	static inline PrefRexW pref_rex_w() { return PrefRexW(); }
	static inline Far far() { return Far(); }
};

// Condition Registers (cr.h)
extern const Cr0234 cr0;
extern const Cr0234 cr2;
extern const Cr0234 cr3;
extern const Cr0234 cr4;
extern const Cr8 cr8;

extern const std::vector<Cr> crs;
extern const std::vector<Cr0234> cr0234s;

// Debug Registers (dr.h)
extern const Dr dr0;
extern const Dr dr1;
extern const Dr dr2;
extern const Dr dr3;
extern const Dr dr4;
extern const Dr dr5;
extern const Dr dr6;
extern const Dr dr7;

extern const std::vector<Dr> drs;

// Eflags (eflag.h)
extern const Eflag cf;
extern const Eflag pf;
extern const Eflag af;
extern const Eflag zf;
extern const Eflag sf;
extern const Eflag tf;
extern const Eflag if_;
extern const Eflag df;
extern const Eflag of;
extern const Eflag iopl0;
extern const Eflag iopl1;
extern const Eflag nt;
extern const Eflag rf;
extern const Eflag vm;
extern const Eflag ac;
extern const Eflag vif;
extern const Eflag vip;
extern const Eflag id;

extern const std::vector<Eflag> eflags;

// Immediates (imm.h)
extern const Zero zero;
extern const One one;
extern const Three three;

// MMX Registers (mm.h)
extern const Mm mm0;
extern const Mm mm1;
extern const Mm mm2;
extern const Mm mm3;
extern const Mm mm4;
extern const Mm mm5;
extern const Mm mm6;
extern const Mm mm7;

extern const std::vector<Mm> mms;

// Modifiers (modifier.h)
extern const Pref66 pref_66;
extern const PrefRexW pref_rexw;
extern const Far far;

} // namespace x64

#endif
