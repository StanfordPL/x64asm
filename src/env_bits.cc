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

#include "src/env_bits.h"

#include <array>
#include <string>
#include <utility>

using namespace std;
using namespace x64asm;

namespace {

const array<string, 22> eflags_ {{
	"%cf", "1", "%pf", "0", "%af", "0", "%zf", "%sf", 
	"%tf", "%if", "%df", "%of", "%iopl[0]", "%iopl[1]", "%nt", "0", 
	"%rf", "%vm", "%ac", "%vif", "%vip", "%id"
}};

const array<string, 16> control_ {{
	"%im", "%dm", "%zm", "%om", "%um", "%pm", "<undef>", "<undef>", 
	"%pc[0]", "%pc[1]", "%rc[0]", "%rc[1]", "%x", "<undef>", "<undef>", "<undef>"
}};

const array<string, 16> status_ {{
	"%ie", "%de", "%ze", "%oe", "%ue", "%pe", "%sf", "%es",
	"%c0", "%c1", "%c2", "%top[0]", "%top[1]", "%top[2]", "%c3", "%b"
}};

const array<string, 16> tag_ {{
	"%tag0[0]", "%tag0[1]", "%tag1[0]", "%tag1[1]", "%tag2[0]", "%tag2[1]", "%tag3[0]", "%tag3[1]",
	"%tag4[0]", "%tag4[1]", "%tag5[0]", "%tag5[1]", "%tag6[0]", "%tag6[1]", "%tag7[0]", "%tag7[1]"
}};

const array<string, 16> mxcsr_ {{
	"%ie", "%de", "%ze", "%oe", "%ue", "%pe", "%daz", "%im",
	"%dm", "%zm", "%om", "%um", "%pm", "%rc[0]", "%rc[1]", "%fz"
}};

} // namespace 

namespace x64asm {

istream& Eflags::read_text(istream& is) {
	string temp;
	is >> temp;

	for (size_t i = 0, ie = eflags_.size(); i < ie; ++i) {
		if (temp == eflags_[i]) {
			index_ = i;
			return is;
		}
	}

	is.setstate(ios::failbit);
	return is;
}

ostream& Eflags::write_text(ostream& os) const {
	if (index() >= eflags_.size()) {
		os.setstate(ios::failbit);
		return os;
	}
	os << eflags_[index()];
	return os;
}

istream& FpuControl::read_text(istream& is) {
	string temp;
	is >> temp;

	for (size_t i = 0, ie = control_.size(); i < ie; ++i) {
		if (temp == control_[i]) {
			index_ = i;
			return is;
		}
	}

	is.setstate(ios::failbit);
	return is;
}

ostream& FpuControl::write_text(ostream& os) const {
	if (index() >= control_.size()) {
		os.setstate(ios::failbit);
		return os;
	}
	os << control_[index()];
	return os;
}

istream& FpuStatus::read_text(istream& is) {
	string temp;
	is >> temp;

	for (size_t i = 0, ie = status_.size(); i < ie; ++i) {
		if (temp == status_[i]) {
			index_ = i;
			return is;
		}
	}

	is.setstate(ios::failbit);
	return is;
}

ostream& FpuStatus::write_text(ostream& os) const {
	if (index() >= status_.size()) {
		os.setstate(ios::failbit);
		return os;
	}
	os << status_[index()];
	return os;
}

istream& FpuTag::read_text(istream& is) {
	string temp;
	is >> temp;

	for (size_t i = 0, ie = tag_.size(); i < ie; ++i) {
		if (temp == tag_[i]) {
			index_ = i;
			return is;
		}
	}

	is.setstate(ios::failbit);
	return is;
}

ostream& FpuTag::write_text(ostream& os) const {
	if (index() >= tag_.size()) {
		os.setstate(ios::failbit);
		return os;
	}
	os << tag_[index()];
	return os;
}

istream& Mxcsr::read_text(istream& is) {
	string temp;
	is >> temp;

	for (size_t i = 0, ie = mxcsr_.size(); i < ie; ++i) {
		if (temp == mxcsr_[i]) {
			index_ = i;
			return is;
		}
	}

	is.setstate(ios::failbit);
	return is;
}

ostream& Mxcsr::write_text(ostream& os) const {
	if (index() >= mxcsr_.size()) {
		os.setstate(ios::failbit);
		return os;
	}
	os << mxcsr_[index()];
	return os;
}

} // namespace x64asm
