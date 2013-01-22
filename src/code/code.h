#ifndef X64_SRC_CODE_CODE_H
#define X64_SRC_CODE_CODE_H

#include <iostream>
#include <vector>

#include "src/code/instruction.h"

namespace x64 {

/** A sequence of Instructions. */
class Code : public std::vector<Instruction> {
	public:
		bool check() const;

		void read_att(std::istream& is);
		void read_intel(std::istream& is);

		void write_att(std::ostream& os) const;
		void write_intel(std::ostream& os) const;
};

} // namespace x64

#endif
