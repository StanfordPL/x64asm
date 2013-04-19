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
	cout << "Usage: trace <code> <state>" << endl;
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

/** Pushes caller save registers. */
void push_caller_save(Assembler& assm) {
	assm.push(rax);
	assm.push(rcx);
	assm.push(rdx);
	assm.push(rsi);
	assm.push(rdi);
	assm.push(r8);
	assm.push(r9);
	assm.push(r10);
	assm.push(r11);
}

/** Pops caller save registers. */
void pop_caller_save(Assembler& assm) {
	assm.pop(r11);
	assm.pop(r10);
	assm.pop(r9);
	assm.pop(r8);
	assm.pop(rdi);
	assm.pop(rsi);
	assm.pop(rdx);
	assm.pop(rcx);
	assm.pop(rax);
}

/** Assemble the set state function. This function only copies the values
	  of registers which are used to pass arguments: rdi, rsi, rdx, rcx, r8, r9, 
		and xmm0-7.
*/
void assm_set_state(Function& set_state, Function& get_state) {
	Assembler assm;
	assm.start(set_state);

	assm.push(rax);
	// Load general purpose registers
	static array<R64,6> rs = {rdi,rsi,rdx,rcx,r8,r9};
	for ( const auto r : rs ) {
		assm.mov(rax, Moffs64{&state.general_purpose[r]});
		assm.mov(r, rax);
	}
	// Load xmm registers
	static array<Xmm,8> xmms = {xmm0,xmm1,xmm2,xmm3,xmm4,xmm5,xmm6,xmm7};
	for ( const auto x : xmms ) {
		assm.mov((R64)rax, Imm64{&state.xmm[x]});
		assm.movdqu(x, M128{rax});
	}
	assm.pop(rax);

	// Now that we've set the state, print it
	assm.push(rsi);
	assm.mov(rsi, Imm64{get_state});
	assm.call(rsi);
	assm.pop(rsi);

	assm.ret();

	assm.finish();
}

/** Assemble the get state function. This function assumes that
    the real value of rsi was pushed onto the stack before it was called.
		Users of the function can assume that it will restore all registers.
*/
void assm_get_state(Function& get_state) {
	Assembler assm;
	assm.start(get_state);

	assm.push(rax);
	// Save every general purpose register except for rsi
	for ( const auto r : r64s )
		if ( r != rsi ) {
			if ( r != rax )
				assm.mov(rax, r);
			assm.mov(Moffs64{&state.general_purpose[r]}, rax);
		}
	// Special treatment for rsi which is on the stack
	assm.mov(rax, M64{rsp, Imm32{16}});
	assm.mov(Moffs64{&state.general_purpose[rsi]}, rax);
	// Save every xmm register
	for ( const auto x : xmms ) {
		assm.mov((R64)rax, Imm64{&state.xmm[x]});
		assm.movdqu(M128{rax}, x);
	}
	assm.pop(rax);

	// Print the state (we don't know what cout does; save everything)
	push_caller_save(assm);
	assm.mov(rsi, Imm64{print_state});
	assm.call(rsi);
	pop_caller_save(assm);

	assm.ret();

	assm.finish();
}

/** Assemble the instrumented code. */
void assm_code(Function& fxn, Function& get_state) {
	Assembler assm;

	assm.start(fxn);
	for ( size_t i = 0, ie = code.size(); i < ie; ++i ) {
		// Emit code to print this instruction
		// (We don't know what cout does; save everything)
		push_caller_save(assm);
		assm.mov(rsi, Imm64{print_instr});
		assm.mov(rdi, Imm64{i});
		assm.call(rsi);
		pop_caller_save(assm);

		// Emit instruction
		assm.assemble(code[i]);

		// Emit code to get state
		assm.push(rsi);
		assm.mov(rsi, Imm64{get_state});
		assm.call(rsi);
		assm.pop(rsi);
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

	Function get_state;
	assm_get_state(get_state);

	Function set_state;
	assm_set_state(set_state, get_state);

	Function fxn;
	assm_code(fxn, get_state);

	set_state.call<int>();
	fxn.call<int>();

	return 0;
}
