#include <array>
#include <fstream>
#include <iostream>

#include "include/x64asm.h"
#include "include/trace.h"

#include "src/def.h"

using namespace std;
using namespace trace;
using namespace x64asm;

/** Global objects (keeping these global makes assembly easier). */
CpuState state;
Code code;

/** Print a usage error and return error code 1. */
int usage() {
  cout << "Usage: trace <code.s> <init.state>" << endl;
  return 1;
}

/** Print a code reading error and return error code 2. */
int code_error() {
  cout << "Unable to read well-formed assembly!" << endl;
  return 2;
}

/** Print a state reading error and return error code 3. */
int state_error() {
  cout << "Unable to read well-formed state!" << endl;
  return 3;
}

/** Prints an instruction from the global code object. */
void print_instr(int idx) {
  cout << Syntax::ATT << code[idx] << endl << endl;
}

/** Prints the global state object. */
void print_state() {
  cout << Format::HEX << state << endl;
}

/** Assemble and instrument the code. */
Function assemble_code(const Function& read_fxn, uint64_t* rax_backup) {
  Assembler assm;
  Function fxn;

	// Reserve enough room for the code and our instrumentation (44 instructions
 	// worth per line of code).
  fxn.reserve(15 * (44 * code.size()));

  // Instrument code
  assm.start(fxn);
  for (size_t i = 0, ie = code.size(); i < ie; ++i) {
		// Backup rax
		assm.mov(Moffs64{rax_backup}, rax);

    // Get state (this restores rax)
		assm.mov((R64)rax, Imm64{read_fxn.get_entrypoint()});
		assm.call(rax);

		// External calls could do anything; push caller save registers.
		assm.mov(M64 {rsp, Imm32{(uint32_t) - 8}},  rax);
		assm.mov(M64 {rsp, Imm32{(uint32_t) - 16}}, rcx);
		assm.mov(M64 {rsp, Imm32{(uint32_t) - 24}}, rdx);
		assm.mov(M64 {rsp, Imm32{(uint32_t) - 32}}, rsi);
		assm.mov(M64 {rsp, Imm32{(uint32_t) - 40}}, rdi);
		assm.mov(M64 {rsp, Imm32{(uint32_t) - 48}}, r8);
		assm.mov(M64 {rsp, Imm32{(uint32_t) - 56}}, r9);
		assm.mov(M64 {rsp, Imm32{(uint32_t) - 64}}, r10);
		assm.mov(M64 {rsp, Imm32{(uint32_t) - 72}}, r11);
		assm.movdqu(M128 {rsp, Imm32{(uint32_t) - 88}},  xmm0);
		assm.movdqu(M128 {rsp, Imm32{(uint32_t) - 104}}, xmm1);
		assm.movdqu(M128 {rsp, Imm32{(uint32_t) - 120}}, xmm2);
		assm.movdqu(M128 {rsp, Imm32{(uint32_t) - 136}}, xmm3);
		assm.movdqu(M128 {rsp, Imm32{(uint32_t) - 152}}, xmm4);
		assm.movdqu(M128 {rsp, Imm32{(uint32_t) - 168}}, xmm5);
		assm.movdqu(M128 {rsp, Imm32{(uint32_t) - 184}}, xmm6);
		assm.movdqu(M128 {rsp, Imm32{(uint32_t) - 200}}, xmm7);
		assm.sub(rsp, Imm32 {200});

		// Call print state then call print instr
		assm.mov((R64)rax, Imm64{print_state});
		assm.call(rax);
		assm.mov((R64)rax, Imm64{print_instr});
		assm.mov(rdi, Imm64{i});
		assm.call(rax);

		// Pop caller save registers, this will restore rax and rdi
		assm.add(rsp, Imm32 {200});
		assm.mov(rax, M64 {rsp, Imm32{(uint32_t) - 8}});
		assm.mov(rcx, M64 {rsp, Imm32{(uint32_t) - 16}});
		assm.mov(rdx, M64 {rsp, Imm32{(uint32_t) - 24}});
		assm.mov(rsi, M64 {rsp, Imm32{(uint32_t) - 32}});
		assm.mov(rdi, M64 {rsp, Imm32{(uint32_t) - 40}});
		assm.mov(r8,  M64 {rsp, Imm32{(uint32_t) - 48}});
		assm.mov(r9,  M64 {rsp, Imm32{(uint32_t) - 56}});
		assm.mov(r10, M64 {rsp, Imm32{(uint32_t) - 64}});
		assm.mov(r11, M64 {rsp, Imm32{(uint32_t) - 72}});
		assm.movdqu(xmm0, M128 {rsp, Imm32{(uint32_t) - 88}});
		assm.movdqu(xmm1, M128 {rsp, Imm32{(uint32_t) - 104}});
		assm.movdqu(xmm2, M128 {rsp, Imm32{(uint32_t) - 120}});
		assm.movdqu(xmm3, M128 {rsp, Imm32{(uint32_t) - 136}});
		assm.movdqu(xmm4, M128 {rsp, Imm32{(uint32_t) - 152}});
		assm.movdqu(xmm5, M128 {rsp, Imm32{(uint32_t) - 168}});
		assm.movdqu(xmm6, M128 {rsp, Imm32{(uint32_t) - 184}});
		assm.movdqu(xmm7, M128 {rsp, Imm32{(uint32_t) - 200}});

    // Emit instruction
    assm.assemble(code[i]);
  }

  assm.finish();

	return fxn;
}

int main(int argc, char** argv) {
	// Check usage
  if (argc != 3) {
    return usage();
  }

	// Read input code
  ifstream ifs1(argv[1]);
  if (!ifs1.is_open()) {
    return code_error();
  }

  ifs1 >> Syntax::ATT >> code;
  if (!ifs1.good()) {
    return code_error();
  }

	// Read input state
	RegSet inputs = RegSet::empty();
	inputs = inputs + rdi + rsi + rdx + rcx + r8 + r9;
	for ( size_t i = 0; i < 8; ++i )
		inputs += SSE_POOL[i];
	CpuState init(inputs);

  ifstream ifs2(argv[2]);
  if (!ifs2.is_open()) {
    return state_error();
  }

  ifs2 >> Format::HEX >> init;
  if (!ifs2.good()) {
    return state_error();
  }

	// Have to reinitialize global state because constructor requires
	// r64s and xmms, which aren't guaranteed initialized when state was.
	state = CpuState();

	// Instrument the input code
	uint64_t rax_backup;
	const auto read_fxn = assemble_read_cpu(state, {rax}, {&rax_backup});
  const auto fxn = assemble_code(read_fxn, &rax_backup);

	// Boot up the initial state and go
	init.write_cpu();
  fxn.call<void>();

  return 0;
}

#include "src/undef.h"
