#ifndef X64_SRC_STREAM_STREAM_H
#define X64_SRC_STREAM_STREAM_H

#include <iostream>
#include <stdint.h>

#include "src/code/code.h"
#include "src/code/cr.h"
#include "src/code/dr.h"
#include "src/code/eflag.h"
#include "src/code/imm.h"
#include "src/code/instruction.h"
#include "src/code/label.h"
#include "src/code/m.h"
#include "src/code/mm.h"
#include "src/code/moffs.h"
#include "src/code/opcode.h"
#include "src/code/r.h"
#include "src/code/rel.h"
#include "src/code/sreg.h"
#include "src/code/st.h"
#include "src/code/xmm.h"
#include "src/code/ymm.h"

namespace x64 {

/** I/O format for iostreams. */
enum class IO : uint32_t {
	ATT = 0,
	DOT,
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
		const Transform t_;
};

/** Transform deactivating manipulator. */
class unset_transform {
 	public:
		inline explicit unset_transform(Transform t) : t_{t} { }
		inline operator int() const { return (int)t_; }
	private:
		const Transform t_;
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

std::ostream& operator<<(std::ostream& os, const x64::Cr0234 c);
std::ostream& operator<<(std::ostream& os, const x64::Cr8 c);
std::ostream& operator<<(std::ostream& os, const x64::Dr d);
std::ostream& operator<<(std::ostream& os, const x64::Eflag e);
std::ostream& operator<<(std::ostream& os, const x64::Imm8 i);
std::ostream& operator<<(std::ostream& os, const x64::Imm16 i);
std::ostream& operator<<(std::ostream& os, const x64::Imm32 i);
std::ostream& operator<<(std::ostream& os, const x64::Imm64 i);
std::ostream& operator<<(std::ostream& os, const x64::Label l);
std::ostream& operator<<(std::ostream& os, const x64::M m);
std::ostream& operator<<(std::ostream& os, const x64::Mm m);
std::ostream& operator<<(std::ostream& os, const x64::Moffs m);
std::ostream& operator<<(std::ostream& os, const x64::NoRexR8 r);
std::ostream& operator<<(std::ostream& os, const x64::RexR8 r);
std::ostream& operator<<(std::ostream& os, const x64::Rl r);
std::ostream& operator<<(std::ostream& os, const x64::Rh r);
std::ostream& operator<<(std::ostream& os, const x64::Rb r);
std::ostream& operator<<(std::ostream& os, const x64::R16 r);
std::ostream& operator<<(std::ostream& os, const x64::R32 r);
std::ostream& operator<<(std::ostream& os, const x64::R64 r);
std::ostream& operator<<(std::ostream& os, const x64::Rel8 r);
std::ostream& operator<<(std::ostream& os, const x64::Rel32 r);
std::ostream& operator<<(std::ostream& os, const x64::Sreg s);
std::ostream& operator<<(std::ostream& os, const x64::St s);
std::ostream& operator<<(std::ostream& os, const x64::Xmm x);
std::ostream& operator<<(std::ostream& os, const x64::Ymm y);

#endif
