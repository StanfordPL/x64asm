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

	cout << "Calling function... " << endl;
	Function fxn(256*1024);
	fxn = assm.assemble(fxn, code);
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

	cout << "Function returned with value " << hex << showbase << res << endl;

	return res;
}



