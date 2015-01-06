/*
Copyright 2013 eric schkufza

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

#include <array>
#include <cassert>
#include <string>

#include "src/constants.h"

using namespace std;
using namespace x64asm;

namespace {

const array<string, 16> rbs_ {{
	"%al", "%cl", "%dl", "%bl", "%spl", "%bpl", "%sil", "%dil",
  "%r8b", "%r9b", "%r10b", "%r11b", "%r12b", "%r13b", "%r14b", "%r15b"
}};

const array<string, 4> rhs_ {{
	"%ah", "%ch", "%dh", "%bh"
}};

const array<string, 16> r16s_ {{
	"%ax", "%cx", "%dx", "%bx", "%sp", "%bp", "%si", "%di",
  "%r8w", "%r9w", "%r10w", "%r11w", "%r12w", "%r13w", "%r14w", "%r15w"
}};

const array<string, 16> r32s_ {{
	"%eax", "%ecx", "%edx", "%ebx", "%esp", "%ebp", "%esi", "%edi",
  "%r8d", "%r9d", "%r10d", "%r11d", "%r12d", "%r13d", "%r14d", "%r15d"
}};

const array<string, 16> r64s_ {{
	"%rax", "%rcx", "%rdx", "%rbx", "%rsp", "%rbp", "%rsi", "%rdi",
  "%r8", "%r9", "%r10", "%r11", "%r12", "%r13", "%r14", "%r15"
}};

} // namespace 

namespace x64asm {

istream& Rb::read_att(istream& is) {
	string temp;
	is >> temp;

	for (size_t i = 0, ie = rbs_.size(); i < ie; ++i) {
		if (temp == rbs_[i]) {
			// note: this is a little hacky until we fix this Rh thing
			val_ = i;
			return is;
		}
	}

	is.setstate(ios::failbit);
	return is;
}

ostream& Rb::write_att(ostream& os) const {
	assert(check());
	return (os << rbs_[val_]);
}

istream& Rl::read_att(istream& is) {
	string temp;
	is >> temp;

	for (size_t i = 0; i < 4; ++i) {
		if (temp == rbs_[i]) {
			// note: this is a little hacky until we fix this Rh thing
			val_ = i;
			return is;
		}
	}

	is.setstate(ios::failbit);
	return is;
}

ostream& Rl::write_att(ostream& os) const {
	assert(check());
	return (os << rbs_[val_]);
}

istream& Rh::read_att(istream& is) {
	string temp;
	is >> temp;

	for (size_t i = 0, ie = rhs_.size(); i < ie; ++i) {
		if (temp == rhs_[i]) {
			*this = rhs[i];
			return is;
		}
	}

	is.setstate(ios::failbit);
	return is;
}

ostream& Rh::write_att(ostream& os) const {
	assert(check());
	return (os << rhs_[val_-4]);
}

istream& R16::read_att(istream& is) {
	string temp;
	is >> temp;

	for (size_t i = 0, ie = r16s_.size(); i < ie; ++i) {
		if (temp == r16s_[i]) {
			*this = r16s[i];
			return is;
		}
	}

	is.setstate(ios::failbit);
	return is;
}

ostream& R16::write_att(ostream& os) const {
	assert(check());
	return (os << r16s_[val_]);
}

istream& R32::read_att(istream& is) {
	string temp;
	is >> temp;

	for (size_t i = 0, ie = r32s_.size(); i < ie; ++i) {
		if (temp == r32s_[i]) {
			*this = r32s[i];
			return is;
		}
	}

	is.setstate(ios::failbit);
	return is;
}

ostream& R32::write_att(ostream& os) const {
	assert(check());
	return (os << r32s_[val_]);
}

istream& R64::read_att(istream& is) {
	string temp;
	is >> temp;

	for (size_t i = 0, ie = r64s_.size(); i < ie; ++i) {
		if (temp == r64s_[i]) {
			*this = r64s[i];
			return is;
		}
	}

	is.setstate(ios::failbit);
	return is;
}

ostream& R64::write_att(ostream& os) const {
	assert(check());
	return (os << r64s_[val_]);
}

ostream& R::write_att(ostream& os) const {
  switch(type()) {
    case Type::RL:
      return static_cast<const Rl* const>(this)->write_att(os);
      break;

    case Type::RB:
    case Type::AL:
    case Type::CL:
      return static_cast<const Rb* const>(this)->write_att(os);
      break;

    case Type::RH:
      return static_cast<const Rh* const>(this)->write_att(os);
      break;

    case Type::R_16:
    case Type::AX:
    case Type::DX:
      return static_cast<const R16 * const>(this)->write_att(os);
      break;

    case Type::R_32:
    case Type::EAX:
      return static_cast<const R32 * const>(this)->write_att(os);
      break;

    case Type::R_64:
    case Type::RAX:
      return static_cast<const R64 * const>(this)->write_att(os);
      break;

    default:
      assert(false);
      return os;
      break;
  }
}

} // namespace x64asm
