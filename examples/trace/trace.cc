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

	Assembler assm;
	Function fxn;

	assm.start(fxn);
	for ( size_t i = 0, ie = code.size(); i < ie; ++i ) {
		// Emit code to print this instruction
		assm.push(rsi);
		assm.push(rdi);
		assm.mov(rsi, Imm64{print_state});
		assm.mov(rdi, Imm64{i});
		assm.call(rsi);
		assm.pop(rdi);
		assm.pop(rsi);

		// Emit instruction
		assm.assemble(code[i]);

		// Emit code to save state

		// Emit code to print state
		//assm.mov(Moffs64{&scratch.rax}, rax);
		//assm.mov(rax, Imm64{print_state});
		//assm.call(rax);
		//assm.mov(rax, Moffs64{&scratch.rax});
	}
	assm.finish();

	fxn.call<int>();

	return 0;
}
