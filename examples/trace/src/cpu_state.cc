/*
Copyright 2103 eric schkufza

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

#include "src/cpu_state.h"

#include <bitset>
#include <iomanip>

#include "def.h"

using namespace std;
using namespace trace;
using namespace x64asm;

namespace {

inline int format_state() {
	static int i = ios_base::xalloc();
	return i;
}

template <typename T>
inline Format get_format(T& ios) {
	return (Format)ios.iword(format_state());
}

template <typename T>
inline void set_format(T& ios, const Format f) {
	ios.iword(format_state()) = (long) f;
}	

void write_bin(ostream& os, uint8_t val) {
	bitset<8> bs(val);
	os << bs.to_string();
}

uint8_t read_bin(istream& is) {
	bitset<8> bs;
	is >> bs;
	return bs.to_ulong();
}

} // namespace 

namespace trace {

CpuState::CpuState(const CpuState& rhs) {
	mask_ = rhs.mask_;
	init_fxns();

	for ( const auto& r : r64s )
		if ( contains(r) )
			gp_[r] = rhs.gp_[r];
	for ( const auto& s : SSE_POOL )
		if ( contains(s) )
			for ( size_t i = 0; i < SSE_SIZE; ++i )
				sse_[SSE_SIZE*s+i] = rhs.sse_[SSE_SIZE*s+i];
}

CpuState& CpuState::operator=(const CpuState& rhs) {
	if ( this == &rhs )
		return *this;

	mask_ = rhs.mask_;
	init_fxns();

	for ( const auto& r : r64s )
		if ( contains(r) )
			gp_[r] = rhs.gp_[r];
	for ( const auto& s : SSE_POOL )
		if ( contains(s) )
			for ( size_t i = 0; i < SSE_SIZE; ++i )
				sse_[SSE_SIZE*s+i] = rhs.sse_[SSE_SIZE*s+i];

	return *this;
}

bool CpuState::operator==(const CpuState& rhs) const {
	if ( mask_ != rhs.mask_ )
		return false;

	for ( const auto& r : r64s )
		if ( contains(r) && gp_[r] != rhs.gp_[r] ) 
			return false;
	for ( const auto& s : SSE_POOL )
		if ( contains(s) )
			for ( size_t i = 0; i < SSE_SIZE; ++i )
				if ( sse_[SSE_SIZE*s+i] != rhs.sse_[SSE_SIZE*s+i] )
					return false;

	return true;
}

CpuState& CpuState::operator&=(const CpuState& rhs) {
	for ( const auto& r : r64s )
		if ( contains(r) && rhs.contains(r) )
			gp_[r] &= rhs.gp_[r];
	for ( const auto& s : SSE_POOL )
		if ( contains(s) && rhs.contains(s) )
			for ( size_t i = 0; i < SSE_SIZE; ++i )
				sse_[SSE_SIZE*s+i] &= rhs.sse_[SSE_SIZE*s+i];
	return *this;
}

CpuState& CpuState::operator|=(const CpuState& rhs) {
	for ( const auto& r : r64s )
		if ( contains(r) && rhs.contains(r) )
			gp_[r] |= rhs.gp_[r];
	for ( const auto& s : SSE_POOL )
		if ( contains(s) && rhs.contains(s) )
			for ( size_t i = 0; i < SSE_SIZE; ++i )
				sse_[SSE_SIZE*s+i] |= rhs.sse_[SSE_SIZE*s+i];
	return *this;
}

CpuState& CpuState::operator^=(const CpuState& rhs) {
	for ( const auto& r : r64s )
		if ( contains(r) && rhs.contains(r) )
			gp_[r] ^= rhs.gp_[r];
	for ( const auto& s : SSE_POOL )
		if ( contains(s) && rhs.contains(s) )
			for ( size_t i = 0; i < SSE_SIZE; ++i )
				sse_[SSE_SIZE*s+i] ^= rhs.sse_[SSE_SIZE*s+i];
	return *this;
}

CpuState CpuState::operator~() const {
	CpuState res;
	for ( const auto& r : r64s )
		if ( contains(r) )
			res.gp_[r] = ~gp_[r];
	for ( const auto& s : SSE_POOL )
		if ( contains(s) )
			for ( size_t i = 0; i < SSE_SIZE; ++i )
				res.sse_[SSE_SIZE*s+i] = ~sse_[SSE_SIZE*s+i];
	return res;
}

Function assemble_write_cpu(const CpuState& cs) {
	Assembler assm;
	Function fxn;

	assm.start(fxn);

	// If we're not writing rax, we should leave it undisturbed (and we
	// use the unused cpu_state space to do it).
	if ( !cs.contains(rax) )
		assm.mov(Moffs64{&cs.gp_[rax]}, rax);
	// Write sse registers
	for ( const auto& s : SSE_POOL )
		if ( cs.contains(s) ) {
			assm.mov((R64)rax, Imm64{&cs.sse_[s*SSE_SIZE]});
			assm.SSE_MOV(s, SSE_MEM{rax});
		}
	// Write gp registers
	for ( const auto& r : r64s )
		if ( cs.contains(r) ) {
			assm.mov(r, Imm64{&cs.gp_[r]});
			assm.mov(r, M64{r});
		}
	// If we're not writing rax, restore its original value
	if ( !cs.contains(rax) )
		assm.mov(rax, Moffs64{&cs.gp_[rax]});

	assm.ret();
	assm.finish();

	return fxn;
}

Function assemble_read_cpu(const CpuState& cs) {
	Assembler assm;
	Function fxn;

	assm.start(fxn);

	// Read rax no matter if we are tracking it or not
	assm.mov(Moffs64{&cs.gp_[rax]}, rax);
	// Read gp registers
	for ( const auto& r : r64s )
		if ( r != rax && cs.contains(r) ) {
			assm.mov(rax, r);
			assm.mov(Moffs64{&cs.gp_[r]}, rax);
		}
	for ( const auto& s : SSE_POOL )
		if ( cs.contains(s) ) {
			assm.mov((R64)rax, Imm64{&cs.sse_[s*SSE_SIZE]});
			assm.SSE_MOV(SSE_MEM{rax}, s);
		}
	// Restore rax so as to be non-destructive
	assm.mov(rax, Moffs64{&cs.gp_[rax]});

	assm.ret();
	assm.finish();

	return fxn;
}

Function assemble_read_cpu(const CpuState& cs, const vector<R64>& rcalls, const vector<uint64_t*>& addrs) {
	Assembler assm;
	Function fxn;

	assm.start(fxn);

	// Restore the values of rcalls from addrs
	assert(rcalls.size() == addrs.size());
	for ( size_t i = 0, ie = rcalls.size(); i < ie; ++i ) {
		assm.mov(rcalls[i], Imm64{addrs[i]});
		assm.mov(rcalls[i], M64{rcalls[i]});
	}
	// Read rax no matter if we are tracking it or not
	assm.mov(Moffs64{&cs.gp_[rax]}, rax);
	// Read gp registers
	for ( const auto& r : r64s )
		if ( r != rax && cs.contains(r) ) {
			assm.mov(rax, r);
			assm.mov(Moffs64{&cs.gp_[r]}, rax);
		}
	for ( const auto& s : SSE_POOL )
		if ( cs.contains(s) ) {
			assm.mov((R64)rax, Imm64{&cs.sse_[s*SSE_SIZE]});
			assm.SSE_MOV(SSE_MEM{rax}, s);
		}
	// Restore rax so as to be non-destructive
	assm.mov(rax, Moffs64{&cs.gp_[rax]});

	assm.ret();
	assm.finish();

	return fxn;
}

} // namespace trace

istream& operator>>(istream& is, Format f) {
	set_format(is, f);
	return is;
}

istream& operator>>(istream& is, CpuState& cs) {
	for ( const auto& r : r64s ) {
		string ignore;
		is >> ignore;
		for ( int i = 7; i >= 0; --i ) {
			int val;
			switch( get_format(is) ) {
				case Format::BIN:
					val = read_bin(is);
					break;
				case Format::HEX:
				default:
					is >> hex >> val;
					break;
			};	
			if ( cs.contains(r) )
				cs.set_fixed_byte(val, r, i);
		}
	}

	for ( const auto& s : SSE_POOL ) {
		string ignore;
		is >> ignore;
		for ( int i = BYTES_PER_SSE-1; i >= 0; --i ) {
			int val;
			switch( get_format(is) ) {
				case Format::BIN:
					val = read_bin(is);
					break;
				case Format::HEX:
				default:
					is >> hex >> val;
					break;
			};	
			if ( cs.contains(s) )
				cs.set_fixed_byte(val, s, i);
		}
	}

	return is;
}

ostream& operator<<(ostream& os, Format f) {
	set_format(os, f);
	return os;
}

ostream& operator<<(ostream& os, const CpuState& cs) {
	for ( const auto& r : r64s ) {
		os << Syntax::ATT << setfill(' ') << r << "\t";
		for ( int i = 7; i >= 0; --i ) {
			int val = cs.contains(r) ? cs.get_fixed_byte(r,i) : 0;
			switch( get_format(os) ) {
				case Format::BIN: 
					write_bin(os, val);
					os << " ";
					break;				
				case Format::HEX:
				default:
					os << hex << noshowbase << setfill('0') << setw(2) << val << " ";
					break;
			}
		}
		os << endl;
	}

	for ( const auto& s : SSE_POOL ) {
		os << Syntax::ATT << s << "\t";
		for ( int i = BYTES_PER_SSE-1; i >= 0; --i ) {
			int val = cs.contains(s) ? cs.get_fixed_byte(s,i) : 0;
			switch( get_format(os) ) {
				case Format::BIN:
				 	write_bin(os, val);
					os << " ";
					break;
				default:
					os << hex << noshowbase << setfill('0') << setw(2) << val << " ";
					break;
			}
		}
		os << endl;
	}

	return os;
}

#include "undef.h"
