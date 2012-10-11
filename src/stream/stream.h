#ifndef X64_SRC_STREAM_STREAM_H
#define X64_SRC_STREAM_STREAM_H

#include <iostream>

#include "src/code/code.h"
#include "src/code/instruction.h"

namespace x64 {

/** iostream input/output formats.
*/
enum FormatVal {
	ATT = 0,
	BIN,
	DOT,
	HEX
};

/** iostream formatting manipulator.
*/
class format {
	public:
		inline explicit format(FormatVal f)
				: f_(f) {
		}
		inline operator FormatVal() const {
			return f_;
		}
	private:
		FormatVal f_;
};

} // namespace x64

std::istream& operator>>(std::istream& is, const x64::format& f);
std::ostream& operator<<(std::ostream& os, const x64::format& f);

std::istream& operator>>(std::istream& is, x64::Code& c);

std::ostream& operator<<(std::ostream& os, const x64::Code& c);
std::ostream& operator<<(std::ostream& os, const x64::Instruction& i);

#endif
