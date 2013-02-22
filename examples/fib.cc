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

// This example demonstrates how the assembler api can be used to create
// functions which call other functions.

// Prints hello world
void hello() {
	cout << "Hello, world!" << endl;
}

int main() {
	// Create an assembler 
	Assembler assm;

	// Example 1:
	// Compile a function that will call hello().
	Code c1 {
		{MOV_R64_IMM64, {rax, Imm64{&hello}}},
		{CALL_R64, {rax}},
		{RET}
	};
	const auto f1 = assm.assemble(c1);

	// Calling the function should invoke hello()
	f1();
	cout << endl;


	return 0;
}

