#ifndef X64ASM_SRC_CODE_H
#define X64ASM_SRC_CODE_H

#include <initializer_list>
#include <iostream>
#include <vector>

#include "src/instruction.h"

namespace x64asm {

/** A sequence of Instructions. */
class Code : public std::vector<Instruction> {
	public:
		inline Code()
				: std::vector<Instruction>{} { }
		inline Code(std::initializer_list<Instruction> is) 
				: std::vector<Instruction>{is} { }
		template <typename InItr>
		inline Code(InItr begin, InItr end) 
				: std::vector<Instruction>{begin, end} { }

		bool check() const;
		void read_att(std::istream& is);
		void read_intel(std::istream& is);
		void write_att(std::ostream& os) const;
		void write_intel(std::ostream& os) const;
};

} // namespace x64asm

#endif
