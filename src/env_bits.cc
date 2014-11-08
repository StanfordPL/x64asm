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

#include "src/constants.h"

using namespace std;
using namespace x64asm;

namespace {

const array<string, 22> eflags_ {{
	"%cf", "<res1>", "%pf", "<res3>", "%af", "<res5>", "%zf", "%sf", 
	"%tf", "%if", "%df", "%of", "%iopl", "%iopl", "%nt", "<res15>", 
	"%rf", "%vm", "%ac", "%vif", "%vip", "%id"
}};

const array<string, 16> control_ {{
	"%im", "%dm", "%zm", "%om", "%um", "%pm", "<res6>", "<res7>", 
	"%pc", "%pc", "%rc", "%rc", "%x", "<res13>", "<res14>", "<res15>"
}};

const array<string, 16> status_ {{
	"%ie", "%de", "%ze", "%oe", "%ue", "%pe", "%sf", "%es",
	"%c0", "%c1", "%c2", "%top", "%top", "%top", "%c3", "%b"
}};

const array<string, 16> tag_ {{
	"%tag0", "%tag0", "%tag1", "%tag1", "%tag2", "%tag2", "%tag3", "%tag3",
	"%tag4", "%tag4", "%tag5", "%tag5", "%tag6", "%tag6", "%tag7", "%tag7"
}};

const array<string, 16> mxcsr_ {{
	"%ie", "%de", "%ze", "%oe", "%ue", "%pe", "%daz", "%im",
	"%dm", "%zm", "%om", "%um", "%pm", "%rc", "%rc", "%fz"
}};

} // namespace 

namespace x64asm {

istream& Eflags::read_text(istream& is) {
	string temp;
	is >> temp;

	for (size_t i = 0, ie = eflags_.size(); i < ie; ++i) {
		if (temp == eflags_[i]) {
			*this = eflags[i];
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
			*this = fpu_control[i];
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
			*this = fpu_status[i];
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
			*this = fpu_tags[i];
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
			*this = mxcsr[i];
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
