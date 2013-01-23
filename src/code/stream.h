#ifndef X64_SRC_CODE_STREAM_H
#define X64_SRC_CODE_STREAM_H

#include <iostream>
#include <stdint.h>

#include "src/code/code.h"
#include "src/code/instruction.h"
#include "src/code/op_set.h"
#include "src/code/operand.h"

namespace x64 {

/** I/O syntax for iostreams. */
enum class Syntax : uint32_t {
	ATT = 0,
	INTEL
};

/** I/O format for iostreams. */
enum class Format : uint32_t {
	TXT = 0,
	ELF,
	HEX,
	DOT
};

/** Transforms for iostreams. */
enum class Transform : uint32_t {
	NONE               = 0x00000000,
	ALL                = 0xffffffff,
	REMOVE_NOP         = 0x00000001,
	REMOVE_UNREACHABLE = 0x00000002
};

/** Syntax manipulator. */
class syntax {
	public:
		inline explicit syntax(Syntax s) : s_{s} { }
		inline operator long() const { return (long)s_; }
		inline bool operator==(Syntax rhs) const { return s_ == rhs; }
	private:
		const Syntax s_;
};

/** Format manipulator. */
class format {
	public:
		inline explicit format(Format f) : f_{f} { }
		inline operator long() const { return (long)f_; }
		inline bool operator==(Format rhs) const { return f_ == rhs; }
	private:
		const Format f_;
};

/** Transform activating manipulator. */
class transform {
 	public:
		inline explicit transform(Transform t) : t_{t} { }
		inline operator long() const { return (long)t_; }
		inline bool operator==(Transform rhs) const { return t_ == rhs; }
	private:
		const Transform t_;
};

} // namespace x64

std::istream& operator>>(std::istream& is, const x64::syntax& s);
std::istream& operator>>(std::istream& is, const x64::format& f);
std::istream& operator>>(std::istream& is, const x64::transform& t);

std::ostream& operator<<(std::ostream& os, const x64::syntax& s);
std::ostream& operator<<(std::ostream& os, const x64::format& f);
std::ostream& operator<<(std::ostream& os, const x64::transform& t);

std::istream& operator>>(std::istream& is, x64::Code& c);

std::ostream& operator<<(std::ostream& os, const x64::Code& c);
std::ostream& operator<<(std::ostream& os, const x64::Instruction& i);
std::ostream& operator<<(std::ostream& os, const x64::Operand& o);
std::ostream& operator<<(std::ostream& os, const x64::OpSet& o);

#endif
