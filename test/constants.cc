#include <iostream>
#include <vector>

#include "include/x64.h"

using namespace x64;
using namespace std;

void foo() {
	cout << "Hello world!  You called this from assembly!" << endl;
}

template <typename T>
void all(const vector<T>& ts) {
	for ( const auto& t : ts )
		if ( cout.fail() )
			cerr << "<FAIL> ";
		else
			cout << t << " ";
	cout << endl;
}

int main() {
	cout << syntax(Syntax::ATT);
	cout << format(Format::TXT);

	cout << "BEGIN WRITING CONSTANTS" << endl;
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
	cout << "DONE WRITING CONSTANTS" << endl;

	return 0;
}
