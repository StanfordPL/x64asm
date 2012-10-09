#include <iomanip>
#include <iostream>

#include "include/x64.h"

using namespace std;
using namespace x64;

void print_gp(const char* name, const State& s, GpReg gp) {
	cout << "  " << name << ": ";
	cout << hex << setw(32) << setfill('0') << s.gp_before(gp);
	cout << " -> ";
	cout << hex << setw(32) << setfill('0') << s.gp_after(gp);
	cout << endl;
}

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

	Tracer tracer;

	for ( auto i = 0; i < 16; ++i )
		tracer.set((GpReg) i);
	for ( size_t i = 0, ie = code.size(); i < ie; ++i ) {
		tracer.set_before(i);
		tracer.set_after(i);
	}

	cout << "Tracing function... " << endl;
	const auto trace = tracer.trace(code);

	cout << "Traced " << trace.size() << " instructions" << endl;

	for ( const auto& state : trace ) {
		const auto l = state.line();
		cout << "Line " << dec << l << ": " << format(ATT) << code.get(l) << endl;
/*	
		print_gp("rax", state, rax);
		print_gp("rcx", state, rcx);
		print_gp("rdx", state, rdx);
		print_gp("rbx", state, rbx);
		print_gp("rsp", state, rsp);
		print_gp("rbp", state, rbp);
		print_gp("rsi", state, rsi);
		print_gp("rdi", state, rdi);
		print_gp(" r8", state, r8);
		print_gp(" r9", state, r9);
		print_gp("r10", state, r10);
		print_gp("r11", state, r11);
		print_gp("r12", state, r12);
		print_gp("r13", state, r13);
		print_gp("r14", state, r14);
		print_gp("r15", state, r15);
*/
		cout << endl;
	}

	return 0;
}




