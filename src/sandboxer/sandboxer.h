#ifndef X64_SRC_SANDBOXER_SANDBOXER_H
#define X64_SRC_SANDBOXER_SANDBOXER_H

#include "src/assembler/assembler.h"
#include "src/assembler/function.h"
#include "src/code/code.h"

namespace x64 {

class Sandbox;

class Sandboxer {
	public:
		inline Function sandbox(Sandbox& s, const Code& code) {
			Function fxn;
			sandbox(fxn, s, code);
			return fxn;
		}

		Function& sandbox(Function& fxn, Sandbox& s, const Code& code);		

	private:
		Assembler assm_;

		void sandbox_runaway(Sandbox& s);
};

} // namespace x64

#endif
