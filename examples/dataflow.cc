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

// This example demonstrates how to use the instruction API to build a 
// dataflow analysis.

// This function iterates over a branch-free code and checks that the read
// set for each instruction is a subset of the def set at that code point.
// The boundary condition to the analysis is provided as an input.
bool undef_read(const Code& code, const RegSet& boundary) {
	RegSet def_set = boundary;

	for ( const auto& instr : code ) {
		// Compute the read set for this instruction.
		const auto read_set = instr.maybe_read_set();

		// Check that the read set is a subset of the def set at this point
		if ( (read_set & def_set) != read_set )
			return true;

		// Compute the transfer function of this instruction
		def_set |= instr.maybe_write_set();
		def_set -= instr.maybe_undef_set();
	}

	// If control has reached here, no undefined reads can occur.
	return false;
}

int main() {
	// Boundary conditions:
	// rax will be assumed live-in to both examples
	RegSet boundary = RegSet::empty();
	boundary += rax;

	// Example 1: No undefined reads.
	Code c1 {
		// write rdi
		{MOV_R64_IMM64, {rdi, Imm64{0}}}, 
		// read rdi; read and write rax
		{ADD_R64_R64, {rax, rdi}},    
		{RET}
	};

	// Performing the analysis on this function should return true.
	cout << c1 << endl;
	cout << endl;
	cout << "Undefined Read Check: ";
 	cout << (undef_read(c1, boundary) ? "true" : "false") << endl;
	cout << endl;

	// Example 2: An undefined read.
	Code c2 {
		// write rdi
		{MOV_R64_IMM64, {rdi, Imm64{0}}}, 
		// read rdi; read and write rax
		{ADD_R64_R64, {rax, rdi}},    
		// undefined read from r8; read and write rax
		{ADD_R64_R64, {rax, r8}},
		{RET}
	};

	// Performing the analsysi on this function should return false.
	cout << c2 << endl;
	cout << endl;
	cout << "Undefined Read Check: ";
 	cout << (undef_read(c2, boundary) ? "true" : "false") << endl;
	cout << endl;

	return 0;
}

