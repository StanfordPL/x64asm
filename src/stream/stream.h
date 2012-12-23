#ifndef X64_SRC_STREAM_STREAM_H
#define X64_SRC_STREAM_STREAM_H

#include <iostream>
#include <stdint.h>

#include "src/code/code.h"
#include "src/code/instruction.h"
#include "src/code/opcode.h"
#include "src/operands/cr.h"
#include "src/operands/dr.h"
#include "src/operands/eflag.h"
#include "src/operands/imm.h"
#include "src/operands/label.h"
#include "src/operands/m.h"
#include "src/operands/mm.h"
#include "src/operands/moffs.h"
#include "src/operands/r.h"
#include "src/operands/rel.h"
#include "src/operands/sreg.h"
#include "src/operands/st.h"
#include "src/operands/xmm.h"
#include "src/operands/ymm.h"

namespace x64 {

/** I/O format for iostreams. */
enum class IO : uint32_t {
	ATT = 0,
	ELF,
	HEX,
	INTEL
};

/** Transforms for iostreams. */
enum class Transform : uint32_t {
	ALL                = 0xffffffff,
	REMOVE_NOP         = 0x00000001,
	REMOVE_UNREACHABLE = 0x00000002
};

/** IO manipulator. */
class set_io {
	public:
		inline explicit set_io(IO io) : io_{io} { }
		inline operator int() const { return (int)io_; }
	private:
		const IO io_;
};

/** Transform activating manipulator. */
class set_transform {
 	public:
		inline explicit set_transform(Transform t) : t_{t} { }
		inline operator int() const { return (int)t_; }
	private:
		const Transform f_;
};

/** Transform deactivating manipulator. */
class unset_transform {
 	public:
		inline explicit unset_transform(Transform t) : t_{t} { }
		inline operator int() const { return (int)t_; }
	private:
		const Transform f_;
};

} // namespace x64

std::istream& operator>>(std::istream& is, const x64::set_io& m);
std::istream& operator>>(std::istream& is, const x64::set_transform& m);
std::istream& operator>>(std::istream& is, const x64::unset_transform& m);

std::ostream& operator<<(std::ostream& os, const x64::set_io& m);
std::ostream& operator<<(std::ostream& os, const x64::set_transform& m);
std::ostream& operator<<(std::ostream& os, const x64::unset_transform& m);

std::istream& operator>>(std::istream& is, x64::Code& c);

std::ostream& operator<<(std::ostream& os, const x64::Code& c);
std::ostream& operator<<(std::ostream& os, const x64::Instruction& i);
std::ostream& operator<<(std::ostream& os, const x64::Opcode o);

std::ostream& operator<<(std::ostream& os, const x64::Cr c);
std::ostream& operator<<(std::ostream& os, const x64::Dr d);
std::ostream& operator<<(std::ostream& os, const x64::Eflag e);
std::ostream& operator<<(std::ostream& os, const x64::Imm i);
std::ostream& operator<<(std::ostream& os, const x64::Label l);
std::ostream& operator<<(std::ostream& os, const x64::M m);
std::ostream& operator<<(std::ostream& os, const x64::Mm m);
std::ostream& operator<<(std::ostream& os, const x64::Moffs m);
std::ostream& operator<<(std::ostream& os, const x64::Rl r);
std::ostream& operator<<(std::ostream& os, const x64::Rh r);
std::ostream& operator<<(std::ostream& os, const x64::Rb r);
std::ostream& operator<<(std::ostream& os, const x64::R16 r);
std::ostream& operator<<(std::ostream& os, const x64::R32 r);
std::ostream& operator<<(std::ostream& os, const x64::R64 r);
std::ostream& operator<<(std::ostream& os, const x64::Rel r);
std::ostream& operator<<(std::ostream& os, const x64::Sreg s);
std::ostream& operator<<(std::ostream& os, const x64::St s);
std::ostream& operator<<(std::ostream& os, const x64::Xmm x);
std::ostream& operator<<(std::ostream& os, const x64::Ymm y);

#endif
