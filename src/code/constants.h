#ifndef X64_SRC_CODE_CONSTANTS_H
#define X64_SRC_CODE_CONSTANTS_H

#include <vector>

#include "src/code/cr.h"
#include "src/code/dr.h"

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



} // namespace x64

#endif
