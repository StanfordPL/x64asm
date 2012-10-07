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
	assm.write_hex(cout, code);
	cout << endl;
	if ( cout.fail() ) {
		cerr << "Error writing output!" << endl;
		return 2;
	}

	return 0;
}



