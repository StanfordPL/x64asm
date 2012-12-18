#ifndef X64_SRC_WRITER_ATT_WRITER_H
#define X64_SRC_WRITER_ATT_WRITER_H

#include <iostream>

#include "src/code/cr.h"
#include "src/code/dr.h"
#include "src/code/eflag.h"
#include "src/code/imm.h"
#include "src/code/label.h"
#include "src/code/mm.h"
#include "src/code/moffs.h"
#include "src/code/r.h"
#include "src/code/rel.h"
#include "src/code/sreg.h"
#include "src/code/st.h"
#include "src/code/xmm.h"
#include "src/code/ymm.h"

namespace x64 {

class AttWriter {
	public:
		static void write(std::ostream& os, Cr c);
		static void write(std::ostream& os, Dr d);
		static void write(std::ostream& os, Eflag e);
		static void write(std::ostream& os, Imm i);
		static void write(std::ostream& os, Label l);
		static void write(std::ostream& os, Mm m);
		static void write(std::ostream& os, Moffs m);
		static void write(std::ostream& os, Rl r);
		static void write(std::ostream& os, Rh r);
		static void write(std::ostream& os, Rb r);
		static void write(std::ostream& os, R16 r);
		static void write(std::ostream& os, R32 r);
		static void write(std::ostream& os, R64 r);
		static void write(std::ostream& os, Rel r);
		static void write(std::ostream& os, Sreg s);
		static void write(std::ostream& os, St s);
		static void write(std::ostream& os, Xmm x);
		static void write(std::ostream& os, Ymm y);
};

} // namespace x64

#endif

