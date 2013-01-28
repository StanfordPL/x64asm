#include <iostream>
#include <vector>

#include "include/x64asm.h"

using namespace x64asm;
using namespace std;

template <typename T>
void all(const vector<T>& ts) {
	for ( const auto& t : ts )
		if ( cout.fail() )
			cerr << "<FAIL> ";
		else
			cout << t << " ";
	cout << endl;
}

void all() {
	all(crs);
	all(drs);
	all(eflags);
	all(mms);
	all(vector<Imm8>{{zero,one,three}});
	all(rhs);
	all(rls);
	all(rbs);
	all(r16s);
	all(r32s);
	all(r64s);
	all(sregs);
	all(sts);
	all(xmms);
	all(ymms);
}

int main() {
	cout << "AT&T Constants:" << endl;
	cout << Syntax::ATT;
	all();
	cout << endl;

	cout << "Intel Constants:" << endl;
	cout << Syntax::INTEL;
	all();
	cout << endl;

	return 0;
}
