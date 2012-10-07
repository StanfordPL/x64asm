#include <iostream>

#include "include/x64.h"

using namespace std;
using namespace x64;

int main() {
	Code code;

	cin >> format(ATT) >> code;
	if ( cin.fail() ) {
		cerr << "Error reading input!" << endl;
		return 1;
	}

	Assembler assm;
	Function fxn;

	cout << "Assembled Hex:" << endl;
	assm.write_hex(cout, code);
	cout << endl << endl;

	cout << "Calling function... " << endl;
	assm.assemble(fxn, code);
	auto res = fxn();
	cout << "Function returned with value " << hex << showbase << res << endl;

	return 0;
}



