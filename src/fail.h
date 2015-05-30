// Copyright 2015 eric schkufza
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#ifndef CPPUTIL_INCLUDE_IO_FAIL_H
#define CPPUTIL_INCLUDE_IO_FAIL_H

#include <iostream>
#include <sstream>

namespace cpputil {

inline int __fail_msg_idx() {
	static const int idx = std::ios::xalloc();
	return idx;
}

inline int __warn_bit_idx() {
	static const int idx = std::ios::xalloc();
	return idx;
}

inline int __warn_msg_idx() {
	static const int idx = std::ios::xalloc();
	return idx;
}

template <typename S>
inline bool failed(S& is) {
	return is.fail();
}

template <typename S>
inline bool warned(S& is) {
  return is.iword(__warn_bit_idx()) != 0;
}

template <typename S>
inline std::ostringstream& fail(S& is) {
	auto& p = is.pword(__fail_msg_idx());
	if (p == nullptr) {
		p = (void*) new std::ostringstream();
	}
	auto ss = static_cast<std::ostringstream*>(p);

	is.setstate(std::ios::failbit);
	return *ss;
}

template <typename S>
inline std::ostringstream& warn(S& is) {
	auto& p = is.pword(__warn_msg_idx());
	if (p == nullptr) {
		p = (void*) new std::ostringstream();
	}
	auto ss = static_cast<std::ostringstream*>(p);

  is.iword(__warn_bit_idx()) = 1;
	return *ss;
}

template <typename S>
inline std::string fail_msg(S& is) {
	auto& p = is.pword(__fail_msg_idx());
	if (p == nullptr) {
		p = (void*) new std::ostringstream();
	}
	const auto& ss = static_cast<std::ostringstream*>(p);

	return ss->str();
}

template <typename S>
inline std::string warn_msg(S& is) {
	auto& p = is.pword(__warn_msg_idx());
	if (p == nullptr) {
		p = (void*) new std::ostringstream();
	}
	const auto& ss = static_cast<std::ostringstream*>(p);

	return ss->str();
}

} // namespace cpputil

#endif
