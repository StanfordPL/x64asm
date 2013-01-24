#ifndef X64_SRC_STREAM_H
#define X64_SRC_STREAM_H

#include <iostream>
#include <stdint.h>

#include "src/cfg.h"
#include "src/code.h"
#include "src/instruction.h"
#include "src/op_set.h"
#include "src/operand.h"

namespace x64 {

/** I/O syntax for iostreams. */
enum class Syntax : uint32_t {
	ATT = 0,
	INTEL
};

/** I/O format for iostreams. */
enum class Format : uint32_t {
	DEBUG = 0,
	DOT,
	ELF,
	HEX,
	TXT,
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

} // namespace x64

std::istream& operator>>(std::istream& is, const x64::syntax& s);
std::istream& operator>>(std::istream& is, const x64::format& f);

std::ostream& operator<<(std::ostream& os, const x64::syntax& s);
std::ostream& operator<<(std::ostream& os, const x64::format& f);

std::istream& operator>>(std::istream& is, x64::Code& c);

std::ostream& operator<<(std::ostream& os, const x64::Cfg& c);
std::ostream& operator<<(std::ostream& os, const x64::Code& c);
std::ostream& operator<<(std::ostream& os, const x64::Instruction& i);
std::ostream& operator<<(std::ostream& os, const x64::Operand& o);
std::ostream& operator<<(std::ostream& os, const x64::OpSet& o);

#endif
