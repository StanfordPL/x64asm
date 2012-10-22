#include <iomanip>
#include <iostream>

#include "include/x64.h"

using namespace std;
using namespace x64;

void print_8(uint8_t x) {
	cout << hex << noshowbase << setfill('0') << setw(2) << (int)x;
}

void print_64(uint64_t x) {
	for ( auto i = 3; i >= 0; --i )
		cout << hex << noshowbase << setfill('0') << setw(4)
			   << ((x >> 16*i) & 0xffff) << " ";
}

void print_128(__uint128_t x) {
	print_64((x >> 64) & 0xffffffffffffffff);
	print_64((x >>  0) & 0xffffffffffffffff);
}

void print_r(const Instruction& instr, const State& s, R64 r) {
	AttWriter w;

	cout << "  " << setfill(' ') << setw(7); w.write(cout, r); cout << ": ";
	print_64(s.r_before(r));
	cout << "-> ";
	if ( instr.is_jump() || instr.is_ret() )
		cout << "???";
	else
		print_64(s.r_after(r));
	cout << endl;
}

void print_cr(const Instruction& instr, const State& s, CondReg cr) {
	AttWriter w;

	cout << "  " << setfill(' ') << setw(7); w.write(cout, cr); cout << ": ";
	cout << s.cond_before(cr) << " -> ";
	if ( instr.is_jump() || instr.is_ret() )
		cout << "???";
	else
		cout << s.cond_after(cr);
	cout << endl;
}

void print_xmm(const Instruction& instr, const State& s, Xmm xmm) {
	AttWriter w;

	cout << "  " << setfill(' ') << setw(7); w.write(cout, xmm); cout << ": ";
	print_128(s.xmm_before(xmm));
	cout << "-> ";
	if ( instr.is_jump() || instr.is_ret() )
		cout << "???";
	else
		print_128(s.xmm_after(xmm));
	cout << endl;
}

void print_mem(const Instruction& instr, const State& state) {
	if ( !state.mem_access() )
		return;

	for ( int i = state.mem_size()-1; i >= 0; --i ) {
		cout << "  "; print_64(state.mem_addr() + i); cout << ": ";
		print_8(state.mem_before(i)); 
		cout << " -> ";
		if ( instr.is_jump() || instr.is_ret() )
			cout << "???";
		else
			print_8(state.mem_after(i));
		cout << endl;
	}
}

int main(int argc, char** argv) {
	Code code;
	cin >> format(ATT) >> code;
	if ( cin.fail() ) {
		cerr << "Error reading input!" << endl;
		return 1;
	}

	cout << "Tracing function... " << endl;
	Tracer tracer;
	Trace trace;
	tracer.trace(trace, code);

	switch ( argc ) {
		case 1: trace();
						break;
		case 2: trace(atoi(argv[1]));
						break;
		case 3: trace(atoi(argv[1]), atoi(argv[2]));
						break;
		case 4: trace(atoi(argv[1]), atoi(argv[2]), atoi(argv[3]));
						break;
		case 5: trace(atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
								  atoi(argv[4]));
						break;
		case 6: trace(atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
								  atoi(argv[4]), atoi(argv[5]));
						break;
		case 7: trace(atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
								  atoi(argv[4]), atoi(argv[5]), atoi(argv[6]));
						break;
		default: cerr << "No support for more than 3 arguments!" << endl;
						 return 2;
	}

	for ( const auto& state : trace ) {
		const auto line = state.line();
		const auto& instr = code[line];

		if ( instr.is_label_defn() )
			continue;

		cout << "Line " << dec << line << ": " << format(ATT) << instr << endl;
		cout << endl;
	
		for ( auto r = R64::begin(), re = R64::end(); r != re; ++r )
			print_r(instr, state, *r);
		cout << endl;

		for ( auto x = Xmm::begin(), xe = Xmm::end(); x != xe; ++x )
			print_xmm(instr, state, *x);
		cout << endl;

		for ( auto c = CondReg::begin(), ce = CondReg::end(); c != ce; ++c )
			print_cr(instr, state, *c);
		cout << endl;

		print_mem(instr, state);
		cout << endl;
	}

	return 0;
}
