/*
Copyright 2013-2015 Stanford University

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

    Rh rh = Constants::ah();
    R8 r8 = Constants::al();
    R16 r16 = Constants::ax();
    R32 r32 = Constants::eax();
    R64 r64 = Constants::rax();
    Xmm xmm = Constants::xmm0();
    Ymm ymm = Constants::ymm0();
    Mm mm = Constants::mm0();
    Eflags ef = Constants::eflags_cf();
    Mxcsr mxcsr = Constants::mxcsr_ie();
    FpuStatus fpustatus = Constants::fpu_status_ie();
    FpuTag fputag = Constants::tag0();
    FpuControl fpucontrol = Constants::fpu_control_im();

    if (s == "...") {
      (*this) = RegSet::universe();
    } else if (istringstream(s) >> r64) {
      (*this) += r64;
    } else if (istringstream(s) >> r32) {
      (*this) += r32;
    } else if (istringstream(s) >> r16) {
      (*this) += r16;
    } else if (istringstream(s) >> r8) {
      (*this) += r8;
    } else if (istringstream(s) >> rh) {
      (*this) += rh;
    } else if (istringstream(s) >> ymm) {
      (*this) += ymm;
    } else if (istringstream(s) >> xmm) {
      (*this) += xmm;
    } else if (istringstream(s) >> mm) {
      (*this) += mm;
    } else if (istringstream(s) >> ef) {
      (*this) += ef;
    } else if (istringstream(s) >> mxcsr) {
      (*this) += mxcsr;
    } else if (istringstream(s) >> fpustatus) {
      (*this) += fpustatus;
    } else if (istringstream(s) >> fputag) {
      (*this) += fputag;
    } else if (istringstream(s) >> fpucontrol) {
      (*this) += fpucontrol;
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
  for(auto mit = mm_begin(); mit != mm_end(); ++mit) {
    os << " " << *mit;
  }
  for (size_t i = 0, ie = Constants::eflags().size(); i < ie; i += Constants::eflags()[i].width()) {
    if (contains(Constants::eflags()[i])) {
      os << " " << Constants::eflags()[i];
    }
  }
  for (size_t i = 0, ie = Constants::fpu_control().size(); i < ie; i += Constants::fpu_control()[i].width()) {
    if (contains(Constants::fpu_control()[i])) {
      os << " " << Constants::fpu_control()[i];
    }
  }
  for (size_t i = 0, ie = Constants::mxcsr().size(); i < ie; i += Constants::mxcsr()[i].width()) {
    if (contains(Constants::mxcsr()[i])) {
      os << " " << Constants::mxcsr()[i];
    }
  }
  for (size_t i = 0, ie = Constants::fpu_status().size(); i < ie; i += Constants::fpu_status()[i].width()) {
    if (contains(Constants::fpu_status()[i])) {
      os << " " << Constants::fpu_status()[i];
    }
  }
  for (size_t i = 0, ie = Constants::fpu_tags().size(); i < ie; i += Constants::fpu_tags()[i].width()) {
    if (contains(Constants::fpu_tags()[i])) {
      os << " " << Constants::fpu_tags()[i];
    }
  }
  os << " }";

  return os;
}

} // namespace x64asm
