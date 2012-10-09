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

	cout << "Assembled Hex:" << endl;
	assm.write_hex(cout, code);
	cout << endl << endl;

	Tracer tracer(code);

	for ( auto i = 0; i < 16; ++i )
		tracer.set((GpReg) i);
	for ( size_t i = 0, ie = code.size(); i < ie; ++i ) {
		tracer.set_before(i);
		tracer.set_after(i);
	}

	Trace trace(1024);

	cout << "Tracing function... " << endl;
	tracer.trace(trace);

	cout << "Traced " << trace.size() << " elements" << endl;

	return 0;
}




