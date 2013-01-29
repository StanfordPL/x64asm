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
	all(control::control);
	all(cr::crs);
	all(dr::drs);
	all(eflags::eflags);
	all(mm::mms);
	all(mxcsr::mxcsr);
	all(vector<Imm8>{imm::zero,imm::one,imm::three});
	all(rh::rhs);
	all(rl::rls);
	all(rb::rbs);
	all(r16::r16s);
	all(r32::r32s);
	all(r64::r64s);
	all(sreg::sregs);
	all(st::sts);
	all(status::status);
	all(tag::tags);
	all(xmm::xmms);
	all(ymm::ymms);
	cout << M8(&sreg::fs, &r32::r15d, &r32::ebx, Scale::TIMES_2, &disp) << endl;
	cout << M16(&sreg::fs, &r32::r15d, &r32::ebx, Scale::TIMES_2, &disp) << endl;
	cout << M32(&sreg::fs, &r32::r15d, &r32::ebx, Scale::TIMES_2, &disp) << endl;
	cout << M64(&sreg::fs, &r32::r15d, &r32::ebx, Scale::TIMES_2, &disp) << endl;
	cout << M128(&sreg::fs, &r32::r15d, &r32::ebx, Scale::TIMES_2, &disp) << endl;
	cout << M256(&sreg::fs, &r32::r15d, &r32::ebx, Scale::TIMES_2, &disp) << endl;
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
		, Instruction{Opcode::ADD_R16_R16, {&r16::ax, &r16::ax}}
	};
	cout << c << endl;

	return 0;
}
