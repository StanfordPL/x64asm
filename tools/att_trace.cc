#include <iomanip>
#include <iostream>

#include "include/x64.h"

using namespace std;
using namespace x64;

void print_gp(const Instruction& instr, 
		          const char* name, const State& s, GpReg gp) {
	cout << "  " << name << ": ";

	cout << hex << noshowbase << setw(32) << setfill('0') << s.gp_before(gp);
	cout << " -> ";

	if ( instr.is_jump() || instr.is_ret() )
		cout << "???";
	else
		cout << hex << noshowbase << setw(32) << setfill('0') << s.gp_after(gp);

	cout << endl;
}

int main(int argc, char** argv) {
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

	Tracer tracer;
	for ( auto i = 0; i < 16; ++i )
		tracer.set((GpReg) i);
	for ( size_t i = 0, ie = code.size(); i < ie; ++i ) {
		tracer.set_before(i);
		tracer.set_after(i);
	}

	cout << "Tracing function... " << endl;
	Trace trace;
	auto fxn = tracer.trace(trace, code);

	switch ( argc ) {
		case 1: fxn();
						break;
		case 2: fxn(atoi(argv[1]));
						break;
		case 3: fxn(atoi(argv[1]), atoi(argv[2]));
						break;
		case 4: fxn(atoi(argv[1]), atoi(argv[2]), atoi(argv[3]));
						break;
		default: cerr << "No support for more than 3 arguments!" << endl;
						 return 2;
	}

	for ( const auto& state : trace ) {
		const auto l = state.line();
		const auto i = code.get(l);

		if ( i.is_label_defn() )
			continue;

		cout << "Line " << dec << l << ": " << format(ATT) << i << endl;
	
		print_gp(i, "rax", state, rax);
		print_gp(i, "rcx", state, rcx);
		print_gp(i, "rdx", state, rdx);
		print_gp(i, "rbx", state, rbx);
		print_gp(i, "rsp", state, rsp);
		print_gp(i, "rbp", state, rbp);
		print_gp(i, "rsi", state, rsi);
		print_gp(i, "rdi", state, rdi);
		print_gp(i, " r8", state, r8);
		print_gp(i, " r9", state, r9);
		print_gp(i, "r10", state, r10);
		print_gp(i, "r11", state, r11);
		print_gp(i, "r12", state, r12);
		print_gp(i, "r13", state, r13);
		print_gp(i, "r14", state, r14);
		print_gp(i, "r15", state, r15);

		cout << endl;
	}

	return 0;
}




