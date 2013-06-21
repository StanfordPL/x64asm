#include <iostream>
#include <string>

#include "include/x64asm.h"

using namespace std;
using namespace x64asm;

/** Prints a usage error. */
int usage_error() {
	cerr << "Usage: x64asm" << endl;
	return 1;
}

/** Prints a parse error. */
int parse_error() {
	cerr << "Unable to read input file!" << endl;
	return 2;
}

/** A simple test program. Reads att syntax and prints human readable hex. */
int main(int argc, char** argv) {
	if ( argc != 1 )
		return usage_error();

	Code c;
	cin >> c;

	if ( !cin.good() )
		return parse_error();

	cout << (Assembler().assemble(c)) << endl;

	return 0;
}
