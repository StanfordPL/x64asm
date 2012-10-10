#include <iomanip>
#include <iostream>

#include "include/x64.h"

using namespace std;
using namespace x64;

int main(int argc, char** argv) {
	Code code;
	cin >> format(ATT) >> code;
	if ( cin.fail() ) {
		cerr << "Error reading input!" << endl;
		return 1;
	}

	Assembler assm;
	cout << "Assembled Hex:" << endl;
	assm.write_hex(cout, code);
	cout << endl << endl;

	Sandboxer sandboxer;
	Sandbox sandbox;

	cout << "Calling function in sandbox... " << endl;
	auto fxn = sandboxer.sandbox(sandbox, code);
	uint64_t res = 0;

	switch ( argc ) {
		case 1: res = fxn();
						break;
		case 2: res = fxn(atoi(argv[1]));
						break;
		case 3: res = fxn(atoi(argv[1]), atoi(argv[2]));
						break;
		case 4: res = fxn(atoi(argv[1]), atoi(argv[2]), atoi(argv[3]));
						break;
		default: cerr << "No support for more than 3 arguments!" << endl;
						 return 2;
	}

	if ( !sandbox.error() )
		cout << "Function returned normally with value " 
			   << hex << showbase << res << endl;
	else {
		cout << "Function returned abnormally" << endl;
		cout << "Control exited abnormally by runaway execution: " 
			   << (sandbox.runaway() ? "yes" : "no") << endl;
	}

	return res;
}
