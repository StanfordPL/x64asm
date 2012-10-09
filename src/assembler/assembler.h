#ifndef X64_SRC_ASSEMBLER_ASSEMBLER_H
#define X64_SRC_ASSEMBLER_ASSEMBLER_H

#include <iostream>
#include <map>

#include "src/assembler/function.h"
#include "src/code/code.h"
#include "src/code/instruction.h"
#include "src/code/label.h"

namespace x64 {

/** 
	A JIT assembler.
*/
class Assembler {
	public:
		inline Function assemble(const Code& code) {
			Function fxn;
			assemble(fxn, code);
			return fxn;
		}

		inline Function& assemble(Function& fxn, const Code& code) {
			start(fxn);
			for ( const auto& instr : code )
				assemble(instr);
			finish();

			return fxn;
		}

		void start(Function& fxn);

		void assemble(const Instruction& instr);

		void finish();

		// void adcb(Imm arg0) ...
		#include "src/gen/assembler.decl"

		void write_binary(std::ostream& os, const Code& code);

		void write_hex(std::ostream& os, const Code& code);

	private:
		std::map<Label, size_t> labels_;
		std::map<size_t, Label> jumps_;

		size_t pos_;
		unsigned char* buf_;

		void start(unsigned char* buffer);
};

} // namespace x64

#endif
