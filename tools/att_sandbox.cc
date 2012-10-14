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

	const auto max_jumps = 1024 * 1024 * 1024;
	sandbox.set_max_jumps(max_jumps);

	cout << "Calling function in sandbox... " << endl;
	Function fxn(256*1024);
	fxn = sandboxer.sandbox(fxn, sandbox, code);
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
		case 5: res = fxn(atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
								      atoi(argv[4]));
						break;
		case 6: res = fxn(atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
								      atoi(argv[4]), atoi(argv[5]));
						break;
		case 7: res = fxn(atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
								      atoi(argv[4]), atoi(argv[5]), atoi(argv[6]));
						break;
		default: cerr << "No support for more than 6 arguments!" << endl;
						 return 2;
	}

	if ( !sandbox.error() )
		cout << "Function returned normally with value " 
			   << hex << showbase << res << endl;
	else {
		cout << "Function returned abnormally" << endl;
		if ( sandbox.runaway() )
			cout << " -> Control exited due to runaway execution";
		if ( sandbox.max_jumps_exceeded() )
			cout << " -> Maximum jumps (" << dec << max_jumps << ") exceeded, "
				   << "likely infinite loop" << endl;
	}

	return res;
}
