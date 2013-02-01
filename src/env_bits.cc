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

#include "env_bits.h"

#include <cassert>

using namespace std;

namespace x64asm {

EnvBits::~EnvBits() {
	// Does nothing.
}

void Eflags::write_att(ostream& os) const {
	os << "%";
	write_intel(os);
}

void Eflags::write_intel(ostream& os) const {
	switch ( index() ) {
		case 0:  os << "eflags[cf]"; break;
		case 2:  os << "eflags[pf]"; break;
		case 4:  os << "eflags[af]"; break;
		case 6:  os << "eflags[zf]"; break;
		case 7:  os << "eflags[sf]"; break;
		case 8:  os << "eflags[tf]"; break;
		case 9:  os << "eflags[if]"; break;
		case 10: os << "eflags[df]"; break;
		case 11: os << "eflags[of]"; break;
		case 13: os << "eflags[iopl]"; break;
		case 14: os << "eflags[nt]"; break;
		case 16: os << "eflags[rf]"; break;
		case 17: os << "eflags[vm]"; break;
		case 18: os << "eflags[ac]"; break;
		case 19: os << "eflags[vif]"; break;
		case 20: os << "eflags[vip]"; break;
		case 21: os << "eflags[id]"; break;

		default:
			assert(false);
	}
}

void FpuControl::write_att(ostream& os) const {
	os << "%";
	write_intel(os);
}

void FpuControl::write_intel(ostream& os) const {
	switch ( index() ) {
		case 0:  os << "fpu_control[im]"; break;
		case 1:  os << "fpu_control[dm]"; break;
		case 2:  os << "fpu_control[zm]"; break;
		case 3:  os << "fpu_control[om]"; break;
		case 4:  os << "fpu_control[um]"; break;
		case 5:  os << "fpu_control[pm]"; break;
		case 9:  os << "fpu_control[pc]"; break;
		case 11: os << "fpu_control[rc]"; break;
		case 12: os << "fpu_control[x]"; break;

		default:
			assert(false);
	}
}

void FpuStatus::write_att(ostream& os) const {
	os << "%";
	write_intel(os);
}

void FpuStatus::write_intel(ostream& os) const {
	switch ( index() ) {
		case 0:  os << "fpu_status[ie]"; break;
		case 1:  os << "fpu_status[de]"; break;
		case 2:  os << "fpu_status[ze]"; break;
		case 3:  os << "fpu_status[oe]"; break;
		case 4:  os << "fpu_status[ue]"; break;
		case 5:  os << "fpu_status[pe]"; break;
		case 6:  os << "fpu_status[sf]"; break;
		case 7:  os << "fpu_status[es]"; break;
		case 8:  os << "fpu_status[c0]"; break;
		case 9:  os << "fpu_status[c1]"; break;
		case 10: os << "fpu_status[c2]"; break;
		case 13: os << "fpu_status[top]"; break;
		case 14: os << "fpu_status[c3]"; break;
		case 15: os << "fpu_status[b]"; break;

		default:
			assert(false);
	}
}

void FpuTag::write_att(ostream& os) const {
	os << "%";
	write_intel(os);
}

void FpuTag::write_intel(ostream& os) const {
	os << "tag(" << ((index()-1)/2) << ")";
}

void Mxcsr::write_att(ostream& os) const {
	os << "%";
	write_intel(os);
}

void Mxcsr::write_intel(ostream& os) const {
	switch ( index() ) {
		case 0:  os << "mxcsr[ie]"; break;
		case 1:  os << "mxcsr[de]"; break;
		case 2:  os << "mxcsr[ze]"; break;
		case 3:  os << "mxcsr[oe]"; break;
		case 4:  os << "mxcsr[ue]"; break;
		case 5:  os << "mxcsr[pe]"; break;
		case 6:  os << "mxcsr[daz]"; break;
		case 7:  os << "mxcsr[im]"; break;
		case 8:  os << "mxcsr[dm]"; break;
		case 9:  os << "mxcsr[zm]"; break;
		case 10: os << "mxcsr[om]"; break;
		case 11: os << "mxcsr[um]"; break;
		case 12: os << "mxcsr[pm]"; break;
		case 14: os << "mxcsr[rc]"; break;
		case 15: os << "mxcsr[fz]"; break;

		default:
			assert(false);
	}
}

} // namespace x64asm

