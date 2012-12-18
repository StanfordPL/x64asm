#include <iostream>

#include "include/x64.h"

using namespace x64;
using namespace std;

template <typename T>
void all(const vector<T>& ts) {
	for ( const auto t : ts )
		cout << t << " ";
	cout << endl;
}

int main() {
	// Condition Registers (cr.h)
	all(vector<Cr>{{cr0,cr2,cr3,cr4,cr8}});
	all(crs);
	all(cr0234s);

	// Debug Registers (dr.h)
	all(vector<Dr>{{dr0,dr1,dr2,dr3,dr4,dr5,dr6,dr7}});
	all(drs);

	return 0;
}
