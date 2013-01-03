#ifndef X64_SRC_ASSEMBLER_ASSEMBLER_H
#define X64_SRC_ASSEMBLER_ASSEMBLER_H

#include <iostream>
#include <unordered_map>
#include <vector>

#include "src/assembler/function.h"
#include "src/code/code.h"
#include "src/code/cr.h"
#include "src/code/dr.h"
#include "src/code/hint.h"
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
		inline void assemble(Function& fxn, const Code& code) {
			start(fxn);
			for ( const auto& instr : code )
				assemble(instr);
			finish();
		}

		void assemble(const Instruction& instr);

		void start(Function& fxn) {
			fxn_ = &fxn;
		}	

		void finish() { 
		}

		#include "src/assembler/assembler.decl"

		inline void bind(Label8 label) {
			// TODO...
		}

		inline void bind(Label32 label) {
			// TODO...
		}

	private:
		Function* fxn_;
		/*
		std::unordered_map<Operand, unsigned char*> labels_;
		std::vector<std::pair<unsigned char*, Operand>> jumps_;
		*/
};

} // namespace x64

#endif
