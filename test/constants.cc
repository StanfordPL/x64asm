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
	all(rls);
	all(rhs);
	all(rbs);
	all(r16s);
	all(r32s);
	all(r64s);
	all(sregs);
	all(mms);
	all(sts);
	all(xmms);
	all(ymms);

	cout << zero << " " << one << " " << three << endl;

	Imm32 disp(100);
	cout << M8(&fs, &r15d, &ebx, Scale::TIMES_2, &disp) << endl;
	cout << M16(&fs, &r15d, &ebx, Scale::TIMES_2, &disp) << endl;
	cout << M32(&fs, &r15d, &ebx, Scale::TIMES_2, &disp) << endl;
	cout << M64(&fs, &r15d, &ebx, Scale::TIMES_2, &disp) << endl;
	cout << M128(&fs, &r15d, &ebx, Scale::TIMES_2, &disp) << endl;
	cout << M256(&fs, &r15d, &ebx, Scale::TIMES_2, &disp) << endl;
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

	cout << sizeof(RegSet) << endl;

	return 0;
}
