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

/** An x64 assembler. */
class Assembler {
	public:
		// State methods
		void start_jit(Function& fxn) { /* TODO */ }
		void finish_jit(Function& fxn) { /* TODO */ }

		void start_hex(std::ostream& os) { /* TODO */ }
		void finish_hex(std::ostream& os) { /* TODO */ }

		void start_elf(std::ostream& os) { /* TODO */ }
		void finish_elf(std::ostream& os) { /* TODO */ }

		// .. add additional output methods here

		// Type-safe Interface
		#include "src/assembler/assembler.decl"

		inline void bind(Label8 label) {
			// TODO...
		}

		inline void bind(Label32 label) {
			// TODO...
		}

		// Type-unsafe Interface
		void assemble(const Instruction& instr) { /* TODO */ }

		inline void assemble(const Code& code) {
			for ( const auto& instr : code )
				assemble(instr);
		}

	private:
		/*
		std::unordered_map<Operand, unsigned char*> labels_;
		std::vector<std::pair<unsigned char*, Operand>> jumps_;

		unsigned char* buf_begin_;
		unsigned char* buf_;

		void start(unsigned char* buffer);
		*/
};

} // namespace x64

#endif
