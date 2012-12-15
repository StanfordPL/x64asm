#ifndef X64_SRC_ASSEMBLER_ASSEMBLER_H
#define X64_SRC_ASSEMBLER_ASSEMBLER_H

#include <iostream>
#include <unordered_map>
#include <vector>

#include "src/assembler/function.h"
#include "src/code/code.h"
#include "src/code/cr.h"
#include "src/code/dr.h"
#include "src/code/imm.h"
#include "src/code/instruction.h"
#include "src/code/label.h"
#include "src/code/m.h"
#include "src/code/mm.h"
#include "src/code/modifier.h"
#include "src/code/moffs.h"
#include "src/code/r.h"
#include "src/code/rel.h"
#include "src/code/sreg.h"
#include "src/code/st.h"
#include "src/code/xmm.h"
#include "src/code/ymm.h"

namespace x64 {

/** A JIT assembler. */
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

		inline void bind(Label label) {
			labels_[label] = buf_;
		}

		// void adcb(Al arg0, Imm8 arg1) ...
		#include "src/assembler/assembler.decl"

		void finish();

		void write_binary(std::ostream& os, const Code& code);

		void write_hex(std::ostream& os, const Code& code);

	private:
		std::unordered_map<Operand, unsigned char*> labels_;
		std::vector<std::pair<unsigned char*, Operand>> jumps_;

		unsigned char* buf_begin_;
		unsigned char* buf_;

		void start(unsigned char* buffer);
};

} // namespace x64

#endif
