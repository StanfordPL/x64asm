#include <iomanip>
#include <iostream>

#include "include/x64.h"

using namespace std;
using namespace x64;

void print_64(uint64_t x) {
	for ( auto i = 3; i >= 0; --i )
		cout << hex << noshowbase << setw(4) << ((x >> 16*i) & 0xffff) << " ";
}

void print_128(__uint128_t x) {
	print_64((x >> 64) & 0xffffffffffffffff);
	print_64((x >>  0) & 0xffffffffffffffff);
}

void print_gp(const Instruction& instr, 
		          const char* name, const State& s, GpReg gp) {
	cout << "  " << name << ": ";
	print_64(s.gp_before(gp));
	cout << "-> ";
	if ( instr.is_jump() || instr.is_ret() )
		cout << "???";
	else
		print_64(s.gp_after(gp));
	cout << endl;
}

void print_cond(const Instruction& instr,
                const char* name, const State& s, CondReg cond) {
	cout << "  " << name << ": ";
	cout << hex << noshowbase << s.cond_before(cond);
	cout << " -> ";
	if ( instr.is_jump() || instr.is_ret() )
		cout << "???";
	else
		cout << hex << noshowbase << s.cond_after(cond);
	cout << endl;
}

void print_xmm(const Instruction& instr, 
		           const char* name, const State& s, XmmReg xmm) {
	cout << "  " << name << ": ";
	print_128(s.xmm_before(xmm));
	cout << "-> ";
	if ( instr.is_jump() || instr.is_ret() )
		cout << "???";
	else
		print_128(s.xmm_after(xmm));
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
	for ( auto i = GpReg::begin(), ie = GpReg::end(); i != ie; ++i )
		tracer.set(*i);
	for ( auto i = CondReg::begin(), ie = CondReg::end(); i != ie; ++i )
		tracer.set(*i);
	for ( auto i = XmmReg::begin(), ie = XmmReg::end(); i != ie; ++i )
		tracer.set(*i);
	for ( size_t i = 0, ie = code.size(); i < ie; ++i ) {
		tracer.set_before(i);
		tracer.set_after(i);
	}

	cout << "Tracing function... " << endl;
	Trace trace;
	Function fxn(256*1024);
	fxn = tracer.trace(fxn, trace, code);

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
		const auto i = code[l];

		if ( i.is_label_defn() )
			continue;

		cout << "Line " << dec << l << ": " << format(ATT) << i << endl;
		cout << endl;
	
		print_gp(i, "rax", state, rax);
		print_gp(i, "rcx", state, rcx);
		print_gp(i, "rdx", state, rdx);
		print_gp(i, "rbx", state, rbx);
		print_gp(i, "rsp", state, rsp);
		print_gp(i, "rbp", state, rbp);
		print_gp(i, "rsi", state, rsi);
		print_gp(i, "rdi", state, rdi);
		print_gp(i, "r8 ", state, r8);
		print_gp(i, "r9 ", state, r9);
		print_gp(i, "r10", state, r10);
		print_gp(i, "r11", state, r11);
		print_gp(i, "r12", state, r12);
		print_gp(i, "r13", state, r13);
		print_gp(i, "r14", state, r14);
		print_gp(i, "r15", state, r15);
		cout << endl;

		print_cond(i, "af", state, af);
		print_cond(i, "cf", state, cf);
		print_cond(i, "of", state, of);
		print_cond(i, "pf", state, pf);
		print_cond(i, "sf", state, sf);
		print_cond(i, "zf", state, zf);
		cout << endl;

		print_xmm(i, "xmm0 ",  state, xmm0);
		print_xmm(i, "xmm1 ",  state, xmm1);
		print_xmm(i, "xmm2 ",  state, xmm2);
		print_xmm(i, "xmm3 ",  state, xmm3);
		print_xmm(i, "xmm4 ",  state, xmm4);
		print_xmm(i, "xmm5 ",  state, xmm5);
		print_xmm(i, "xmm6 ",  state, xmm6);
		print_xmm(i, "xmm7 ",  state, xmm7);
		print_xmm(i, "xmm8 ",  state, xmm8);
		print_xmm(i, "xmm9 ",  state, xmm9);
		print_xmm(i, "xmm10", state, xmm10);
		print_xmm(i, "xmm11", state, xmm11);
		print_xmm(i, "xmm12", state, xmm12);
		print_xmm(i, "xmm13", state, xmm13);
		print_xmm(i, "xmm14", state, xmm14);
		print_xmm(i, "xmm15", state, xmm15);
		cout << endl;
	}

	return 0;
}




