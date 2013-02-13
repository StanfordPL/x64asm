/*
Copyright 2103 eric schkufza

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

#include <fstream>
#include <iostream>
#include "include/x64asm.h"

using namespace std;
using namespace x64asm;

// This program demonstrates three different methods of assembling and
// invoking a toy implementation of the memcpy function in this directory,
// memcpy.s

// Example 1: Read from file 
Function from_file() {
	// Create an assembler.
	Assembler assm;

	// Create a code object and read its contents using the >> operator.
	// In addition to fstreams, this works for all istream types.
	Code c;
	ifstream ifs("memcpy.s");
	ifs >> c;
	
	// Assemble the code and return the result.
	return assm.assemble(c);
}

// Example 2: Write code using in-memory RTL.
Function from_code() {
	// Create an assembler.
	Assembler assm;

	// Create code using the in-memory RTL.
	// In addition to the initializer list constructor method shown below,
	// the Code class supports all all STL sequence container operations
	// (ie: resize, find, clear...).
	Code c {
		{LABEL_DEFN, {Label{"loop"}}},
		{CMP_R64_IMM32, {rdx, Imm32{0}}},
		{JE_LABEL, {Label{"done"}}},
		{MOV_RL_M8, {al, M8{rsi}}},
		{MOV_M8_RL, {M8{rdi}, al}},
		{DEC_R64, {rdx}},
		{JMP_LABEL, {Label{"loop"}}},
		{LABEL_DEFN, {Label{"done"}}},
		{RET, {}}
	};

	// Assemble the code and return the result.
	return assm.assemble(c);
}

// Example 3: Use the assembler API.
Function from_api() {
	// Create an assembler and a function to compile code to.
	Assembler assm;
	Function memcpy;

	// The assembler is stateful, this method specializes it for a function.
	assm.start(memcpy);

	// Instructions are inserted using type-safe API calls.
	// Note that labels do not need to be bound prior to being referenced.
	assm.bind(Label{"loop"});	
	assm.cmp(rdx, Imm32{0});
	assm.je(Label{"done"});

	assm.mov(al, M8{rsi});
	assm.mov(M8{rdi}, al);
	assm.dec(rdx);
	assm.jmp(Label{"loop"});

	assm.bind(Label{"done"});
	assm.ret();

	// Finish assembly (ie: patch up label references) and return the result.
	assm.finish();
	return memcpy;
}

// Example: Invokes an assembled version of the memcpy function.

void test(const Function& memcpy) {
	const char* source = "Hello, world!";
	char* target = new char[32];

	// A function can be called with up to six arguments, each of which must
	// be castable to a unit64_t.  No explicit conversion is necessary.
	memcpy(target, source, 14);

	cout << "Should print \"Hello, world!\": [" << target << "]" << endl;

	delete target;
}

int main() {
	Function f1 = from_file();
	test(f1);

	Function f2 = from_code();
	test(f2);

	Function f3 = from_api();
	test(f3);

	return 0;
}
