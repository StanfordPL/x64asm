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

#include "src/reg_set.h"

#include <sstream>
#include <string>

#include "src/alias.h"
#include "src/constants.h"

using namespace std;

namespace x64asm {



istream& RegSet::read_text(istream& is) {
	*this = RegSet::empty();
	string s;

	is >> s;
	if (s != "{") {
		is.setstate(ios::failbit);
		return is;
	}	

	while (true) {
		is >> s;
		if (s == "}") {
			break;
		}

    Rh rh = ah;
    Rb r8 = al;
    R16 r16 = ax; 
    R32 r32 = eax;
    R64 r64 = rax;
    Xmm xmm = xmm0;
    Ymm ymm = ymm0;
		Eflags ef = eflags_cf;

		if (istringstream(s) >> r64) {
			(*this) += r64;
		}	else if (istringstream(s) >> r32) {
			(*this) += r32;
		}	else if (istringstream(s) >> r16) {
			(*this) += r16;
		}	else if (istringstream(s) >> r8) {
			(*this) += r8;
		}	else if (istringstream(s) >> rh) {
			(*this) += rh;
		}	else if (istringstream(s) >> ymm) {
			(*this) += ymm;
		}	else if (istringstream(s) >> xmm) {
			(*this) += xmm;
		}	else if (istringstream(s) >> ef) {
			(*this) += ef;
		} else {
			is.setstate(ios::failbit);
			return is;
		}
	}

	return is;
}

ostream& RegSet::write_text(ostream& os) const {
  os << "{";
  for(auto rit = gp_begin(); rit != gp_end(); ++rit) {
    os << " " << *rit;
  }
  for(auto sit = sse_begin(); sit != sse_end(); ++sit) {
    os << " " << *sit;
  }
	for (size_t i = 0, ie = eflags.size(); i < ie; i += eflags[i].width()) {
		if (contains(eflags[i])) {
			os << " " << eflags[i];
		}
	}
  os << " }";

	return os;
}

RegSet::GpIterator& RegSet::GpIterator::operator++() {
  if(finished_)
    return *this;

  bool found = false;

  for(; index_ < r64s.size() && !found; index_++) {

    if(rs_->contains(r64s[index_])) {
      current_ = r64s[index_];
      found = true;
      break;
    }
    if(rs_->contains(r32s[index_])) {
      current_ = r32s[index_];
      found = true;
      break;
    }
    if(rs_->contains(r16s[index_])) {
      current_ = r16s[index_];
      found = true;
      break;
    }
    if(index_ < 4 && rs_->contains(rhs[index_])) {
      current_ = rhs[index_];
      found = true;
      break;
    }
    if(index_ < 4 && rs_->contains(rls[index_])) {
      current_ = rls[index_];
      found = true;
      break;
    }
    if(index_ >= 4 && rs_->contains(rbs[index_-4])) {
      current_ = rbs[index_-4];
      found = true;
      break;
    }
  }
  
  if (!found) {
    finished_ = true;
  }

  index_++;

  return *this;
} 

RegSet::SseIterator& RegSet::SseIterator::operator++() {
  if(finished_)
    return *this;

  bool found = false;

  for(; index_ < ymms.size() && !found; index_++) {

    if(rs_->contains(ymms[index_])) {
      current_ = ymms[index_];
      found = true;
      break;
    }
    if(rs_->contains(xmms[index_])) {
      current_ = xmms[index_];
      found = true;
      break;
    }
  }
  
  if (!found)
    finished_ = true;

  index_++;

  return *this;
}

RegSet::FlagsIterator& RegSet::FlagsIterator::operator++() {
  if(finished_)
    return *this;

  bool found = false;

  for(; index_ < eflags.size() && !found; index_++) {

    if(rs_->contains(eflags[index_]) &&
       (index_ == 0 || index_ == 2 || index_ == 4 ||
        index_ == 6 || index_ == 7 || index_ == 11)) {
      current_ = eflags[index_];
      found = true;
      break;
    }
  }
  
  if (!found)
    finished_ = true;

  index_++;

  return *this;
}


} // namespace x64asm
