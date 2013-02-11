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

#include "src/r.h"

#include "src/reg_set.h"

#include <cassert>

using namespace std;

namespace x64asm {

R64 R::parent() const {
	return R64{val_};
}

void Rl::write_att(ostream& os) const {
	os << "%";
	write_intel(os);
}

void Rl::write_intel(ostream& os) const {
	switch ( val_ ) {
		case 0:  os << "al"; break;
		case 1:  os << "cl"; break;
		case 2:  os << "dl"; break;
		case 3:  os << "bl"; break;

		default: assert(false);
	}
}

R64 Rh::parent() const {
	return R64{val_-4};
}

void Rh::write_att(ostream& os) const {
	os << "%";
	write_intel(os);
}

void Rh::write_intel(ostream& os) const {
	switch ( val_ ) {
		case 4:  os << "ah"; break;
		case 5:  os << "ch"; break;
		case 6:  os << "dh"; break;
		case 7:  os << "bh"; break;

		default: assert(false);
	}
}

void Rb::write_att(ostream& os) const {
	os << "%";
	write_intel(os);
}

void Rb::write_intel(ostream& os) const {
	switch ( val_ ) {
		case 4:  os << "spl"; break;
		case 5:  os << "bpl"; break;
		case 6:  os << "sil"; break;
		case 7:  os << "dil"; break;
		case 8:  os << "r8b"; break;
		case 9:  os << "r9b"; break;
		case 10: os << "r10b"; break;
		case 11: os << "r11b"; break;
		case 12: os << "r12b"; break;
		case 13: os << "r13b"; break;
		case 14: os << "r14b"; break;
		case 15: os << "r15b"; break;

		default: assert(false);
	}
}

void R16::write_att(ostream& os) const {
	os << "%";
	write_intel(os);
}

void R16::write_intel(ostream& os) const {
	switch ( val_ ) {
		case 0:  os << "ax"; break;
		case 1:  os << "cx"; break;
		case 2:  os << "dx"; break;
		case 3:  os << "bx"; break;
		case 4:  os << "sp"; break;
		case 5:  os << "bp"; break;
		case 6:  os << "si"; break;
		case 7:  os << "di"; break;
		case 8:  os << "r8w"; break;
		case 9:  os << "r9w"; break;
		case 10: os << "r10w"; break;
		case 11: os << "r11w"; break;
		case 12: os << "r12w"; break;
		case 13: os << "r13w"; break;
		case 14: os << "r14w"; break;
		case 15: os << "r15w"; break;

		default: assert(false);
	}
}

void R32::write_att(ostream& os) const {
	os << "%";
	write_intel(os);
}

void R32::write_intel(ostream& os) const {
	switch ( val_ ) {
		case 0:  os << "eax"; break;
		case 1:  os << "ecx"; break;
		case 2:  os << "edx"; break;
		case 3:  os << "ebx"; break;
		case 4:  os << "esp"; break;
		case 5:  os << "ebp"; break;
		case 6:  os << "esi"; break;
		case 7:  os << "edi"; break;
		case 8:  os << "r8d"; break;
		case 9:  os << "r9d"; break;
		case 10: os << "r10d"; break;
		case 11: os << "r11d"; break;
		case 12: os << "r12d"; break;
		case 13: os << "r13d"; break;
		case 14: os << "r14d"; break;
		case 15: os << "r15d"; break;

		default: assert(false);
	}
}

void R64::write_att(ostream& os) const {
	os << "%";
	write_intel(os);
}

void R64::write_intel(ostream& os) const {
	switch ( val_ ) {
		case 0:  os << "rax"; break;
		case 1:  os << "rcx"; break;
		case 2:  os << "rdx"; break;
		case 3:  os << "rbx"; break;
		case 4:  os << "rsp"; break;
		case 5:  os << "rbp"; break;
		case 6:  os << "rsi"; break;
		case 7:  os << "rdi"; break;
		case 8:  os << "r8"; break;
		case 9:  os << "r9"; break;
		case 10: os << "r10"; break;
		case 11: os << "r11"; break;
		case 12: os << "r12"; break;
		case 13: os << "r13"; break;
		case 14: os << "r14"; break;
		case 15: os << "r15"; break;

		default: assert(false);
	}
}

} // namespace x64asm
