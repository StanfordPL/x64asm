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
	all(crs);
	all(cr0234s);
	all(drs);
	all(eflags);
	all(vector<Imm>{{zero, one, three}});

	return 0;
}
