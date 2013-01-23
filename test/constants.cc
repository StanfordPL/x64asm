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

	cout << "Parent register for " << xmm1 << " = " << xmm1.parent() << endl;
	cout << "Parent register for " << al << " = " << al.parent() << endl;
	cout << "Parent register for " << ah << " = " << ah.parent() << endl;
	cout << "Parent register for " << ax << " = " << ax.parent() << endl;
	cout << "Parent register for " << eax << " = " << eax.parent() << endl;
	cout << "Parent register for " << rax << " = " << rax.parent() << endl;

	OpSet os = OpSet::empty();
	os += al;
	os += ah;
	os += ax;
	os += eax;
	os += rax;

	os += ch;
	os += dl;
	os += spl;
	os += mm3;
	os += xmm2;
	os += ymm2;
	os += ymm4;

	xmm8.insert_in(os, true);
	r8b.insert_in(os, true);

	cout << os << endl;
	cout << OpSet::universe() << endl;

	return 0;
}
