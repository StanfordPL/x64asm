#ifndef X64_SRC_ASSEMBLER_ASSEMBLER_H
#define X64_SRC_ASSEMBLER_ASSEMBLER_H

#include <iostream>
#include <unordered_map>
#include <vector>

#include "src/assembler/function.h"
#include "src/assembler/code.h"
#include "src/assembler/cr.h"
#include "src/assembler/dr.h"
#include "src/assembler/imm.h"
#include "src/assembler/instruction.h"
#include "src/assembler/label.h"
#include "src/assembler/m.h"
#include "src/assembler/mm.h"
#include "src/assembler/modifier.h"
#include "src/assembler/moffs.h"
#include "src/assembler/r.h"
#include "src/assembler/rel.h"
#include "src/assembler/sreg.h"
#include "src/assembler/st.h"
#include "src/assembler/xmm.h"
#include "src/assembler/ymm.h"

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
