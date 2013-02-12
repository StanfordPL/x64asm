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

#include <iostream>
#include "include/x64asm.h"

using namespace std;
using namespace x64asm;

// This example shows how to assemble and execute a toy implementation
// of the memcpy function, shown below, using the assembler API.
//
// <rdi> = target
// <rsi> = source
// <rdx> = n
//
// .loop:
//   cmpq $0x0, %rdx
//   je .done
//
//   movb (%rsi), %al
//   movb %al, (%rdi)
//   decq %rdx
//   jmp .loop
//
// .done:
//   retq

int main() {
	// Declare a function. By default this allocates an executable 1KB page.
	Function memcpy;	

	// Declare an assembler.
	Assembler assm;

	// Declare labels
	// Conceptually distinct labels should be assigned unique integer values.
	Label loop{0};
	Label done{1};

	// The start() method configures the assembler to emit code into memcpy.
	assm.start(memcpy);

	// Emit code.  Note that the assembler API follows the intel convention
	// for ordering operands.  This is the opposite of the (AT&T) code shown
	// above.  Also note that labels do not have to be bound prior to reference.
	assm.bind(loop);	
		assm.cmp(rdx, Imm32{0});
		assm.je(done);

		assm.mov(al, M8{rsi});
		assm.mov(M8{rdi}, al);
		assm.dec(rdx);
		assm.jmp(loop);

	assm.bind(done);
		assm.ret();

	// The finish() method finalizes code generation (ie: label resolution)
	assm.finish();

	Code c {
		{LABEL_DEFN, {loop}},
		{CMP_R64_IMM32, {rdx, Imm32{0}}},
		{JE_LABEL, {done}},
		{MOV_RL_M8, {al, M8{rsi}}},
		{MOV_M8_RL, {M8{rdi}, al}},
		{DEC_R64, {rdx}},
		{JMP_LABEL, {loop}},
		{LABEL_DEFN, {done}},
		{RET, {}}
	};
	assm.write_hex(cout, c);

	// Allocate some buffers and try out the code.
	const char* source = "Hello, world!";
	char* target = new char[32];

	memcpy(target, source, 14);
	cout << "Target: [" << target << "]" << endl;

	delete target;

	return 0;
}
