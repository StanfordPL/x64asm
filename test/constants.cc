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
	Imm32 disp(100);
	all(crs);
	all(drs);
	all(eflags);
	all(mms);
	all(vector<Imm8>{zero,one,three});
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
	all(vector<M>{  M8(&fs, &r15d, &ebx, Scale::TIMES_2, &disp),
	               M16(&fs, &r15d, &ebx, Scale::TIMES_2, &disp),
	               M32(&fs, &r15d, &ebx, Scale::TIMES_2, &disp),
 	               M64(&fs, &r15d, &ebx, Scale::TIMES_2, &disp),
	              M128(&fs, &r15d, &ebx, Scale::TIMES_2, &disp),
	              M256(&fs, &r15d, &ebx, Scale::TIMES_2, &disp)});
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


	cout << Syntax::ATT;
	Code c {
 		  Instruction{Opcode::NOP}
		, Instruction{Opcode::ADD_R16_R16, {&ax, &ax}}
	};
	cout << c << endl;

	return 0;
}
