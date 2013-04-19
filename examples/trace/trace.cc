#include <array>
#include <fstream>
#include <iostream>

#include "include/x64asm.h"

#include "state.h"

using namespace std;
using namespace trace;
using namespace x64asm;

/** Global objects (keeping these global makes assembly easier). */
State state;
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
	cout << code[idx] << endl << endl;
}

/** Prints the global state object. */
void print_state() {
	cout << state << endl << endl;
}

/** Push caller save registers. */
void push_caller_save(Assembler& assm) {
	assm.mov(M64{rsp,Imm32{(uint32_t)-8}},  rax);
	assm.mov(M64{rsp,Imm32{(uint32_t)-16}}, rcx);
	assm.mov(M64{rsp,Imm32{(uint32_t)-24}}, rdx);
	assm.mov(M64{rsp,Imm32{(uint32_t)-32}}, rsi);
	assm.mov(M64{rsp,Imm32{(uint32_t)-40}}, rdi);
	assm.mov(M64{rsp,Imm32{(uint32_t)-48}}, r8);
	assm.mov(M64{rsp,Imm32{(uint32_t)-56}}, r9);
	assm.mov(M64{rsp,Imm32{(uint32_t)-64}}, r10);
	assm.mov(M64{rsp,Imm32{(uint32_t)-72}}, r11);

	assm.movdqu(M128{rsp,Imm32{(uint32_t)-88}},  xmm0);
	assm.movdqu(M128{rsp,Imm32{(uint32_t)-104}}, xmm1);
	assm.movdqu(M128{rsp,Imm32{(uint32_t)-120}}, xmm2);
	assm.movdqu(M128{rsp,Imm32{(uint32_t)-136}}, xmm3);
	assm.movdqu(M128{rsp,Imm32{(uint32_t)-152}}, xmm4);
	assm.movdqu(M128{rsp,Imm32{(uint32_t)-168}}, xmm5);
	assm.movdqu(M128{rsp,Imm32{(uint32_t)-184}}, xmm6);
	assm.movdqu(M128{rsp,Imm32{(uint32_t)-200}}, xmm7);

	assm.sub(rsp, Imm32{200});
}

/** Pop caller save registers. */
void pop_caller_save(Assembler& assm) {
	assm.add(rsp, Imm32{200});

	assm.mov(rax, M64{rsp,Imm32{(uint32_t)-8}});
	assm.mov(rcx, M64{rsp,Imm32{(uint32_t)-16}});
	assm.mov(rdx, M64{rsp,Imm32{(uint32_t)-24}});
	assm.mov(rsi, M64{rsp,Imm32{(uint32_t)-32}});
	assm.mov(rdi, M64{rsp,Imm32{(uint32_t)-40}});
	assm.mov(r8,  M64{rsp,Imm32{(uint32_t)-48}});
	assm.mov(r9,  M64{rsp,Imm32{(uint32_t)-56}});
	assm.mov(r10, M64{rsp,Imm32{(uint32_t)-64}});
	assm.mov(r11, M64{rsp,Imm32{(uint32_t)-72}});

	assm.movdqu(xmm0, M128{rsp,Imm32{(uint32_t)-88}});
	assm.movdqu(xmm1, M128{rsp,Imm32{(uint32_t)-104}});
	assm.movdqu(xmm2, M128{rsp,Imm32{(uint32_t)-120}});
	assm.movdqu(xmm3, M128{rsp,Imm32{(uint32_t)-136}});
	assm.movdqu(xmm4, M128{rsp,Imm32{(uint32_t)-152}});
	assm.movdqu(xmm5, M128{rsp,Imm32{(uint32_t)-168}});
	assm.movdqu(xmm6, M128{rsp,Imm32{(uint32_t)-184}});
	assm.movdqu(xmm7, M128{rsp,Imm32{(uint32_t)-200}});
}

/** Assemble the get state function. Assumes that the real value of rsi was 
	  pushed onto the stack before it was called. Restores all registers.
*/
void assm_read_state(Function& read_state) {
	Assembler assm;
	assm.start(read_state);

	assm.push(rax);
	// Save every general purpose register except for rsi
	for ( const auto r : r64s )
		if ( r != rsi ) {
			if ( r != rax )
				assm.mov(rax, r);
			assm.mov(Moffs64{&state.general_purpose[r]}, rax);
		}
	// Special treatment for rsi which is on the stack (above return addr)
	assm.mov(rax, M64{rsp, Imm32{16}});
	assm.mov(Moffs64{&state.general_purpose[rsi]}, rax);
	// Save every xmm register
	for ( const auto x : xmms ) {
		assm.mov((R64)rax, Imm64{&state.xmm[x]});
		assm.movdqu(M128{rax}, x);
	}
	assm.pop(rax);

	// Print the state (external call; save everything)
	push_caller_save(assm);
	assm.mov(rsi, Imm64{print_state});
	assm.call(rsi);
	pop_caller_save(assm);

	assm.ret();
	assm.finish();
}

/** Assemble and instrument the code. */
void assm_code(Function& fxn, Function& read_state) {
	Assembler assm;
	assm.start(fxn);

	// Load general purpose argument regs with user input (in global state object)
	static array<R64,6> rin = {rdi,rsi,rdx,rcx,r8,r9};
	for ( const auto r : rin ) {
		assm.mov(rax, Moffs64{&state.general_purpose[r]});
		assm.mov(r, rax);
	}
	// Load xmm argument regs with user input (in global state object)
	static array<Xmm,8> xin = {xmm0,xmm1,xmm2,xmm3,xmm4,xmm5,xmm6,xmm7};
	for ( const auto x : xin ) {
		assm.mov((R64)rax, Imm64{&state.xmm[x]});
		assm.movdqu(x, M128{rax});
	}

	// Instrument code
	for ( size_t i = 0, ie = code.size(); i < ie; ++i ) {
		// Emit code to get state
		assm.push(rsi);
		assm.mov(rsi, Imm64{read_state});
		assm.call(rsi);
		assm.pop(rsi);

		// Emit code to print this instruction (external call; save everything)`
		push_caller_save(assm);
		assm.mov(rsi, Imm64{print_instr});
		assm.mov(rdi, Imm64{i});
		assm.call(rsi);
		pop_caller_save(assm);

		// Emit instruction
		assm.assemble(code[i]);
	}

	assm.finish();
}

int main(int argc, char** argv) {
	if ( argc != 3 )
		return usage();

	ifstream ifs1(argv[1]);
	if ( !ifs1.is_open() )
		return code_error();
	
	ifs1 >> code;
	if ( !ifs1.good() )
		return code_error();

	ifstream ifs2(argv[2]);
	if ( !ifs2.is_open() )
		return state_error();

	ifs2 >> state;
	if ( !ifs2.good() )
		return state_error();

	Function read_state;
	assm_read_state(read_state);

	Function fxn;
	fxn.reserve(15*(24+44*code.size()));
	assm_code(fxn, read_state);

	fxn.call<int>();

	return 0;
}
