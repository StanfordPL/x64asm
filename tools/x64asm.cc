#include <iostream>
#include <string>

#include "include/x64asm.h"

using namespace std;
using namespace x64asm;

int usage_error() {
	cerr << "Usage: x64asm -i (att|intel) -o (att|intel|hex)" << endl;
	return 1;
}

int parse_error() {
	cerr << "Unable to read input file!" << endl;
	return 2;
}

int main(int argc, char** argv) {
	if ( argc != 5 )
		return usage_error();
	if ( string(argv[1]) != "-i" )
		return usage_error();
	if ( string(argv[3]) != "-o" )
		return usage_error();

	Code c;

	string in_type = argv[2];
	if ( in_type == "att" )
		cin >> Syntax::ATT >> c;
	else if ( in_type == "intel" )
		cin >> Syntax::INTEL >> c;
	else 
		usage_error();

	if ( !cin.good() )
		return parse_error();

	string out_type = argv[4];
	if ( out_type == "att" )
		cout << Syntax::ATT << c << endl;
	else if ( out_type == "intel" )
		cout << Syntax::INTEL << c << endl;
	else if ( out_type == "hex" )
		cout << (Assembler().assemble(c)) << endl;
	else
		usage_error();

	return 0;
}
