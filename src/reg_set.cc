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

    Rh rh = Constants::ah();
    Rb r8 = Constants::al();
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

    istringstream iss;
    if (s == "...") {
      (*this) = RegSet::universe();
    } else if (iss.str(s), r64.read_att(iss)) {
      (*this) += r64;
    } else if (iss.str(s), r32.read_att(iss)) {
      (*this) += r32;
    } else if (iss.str(s), r16.read_att(iss)) {
      (*this) += r16;
    } else if (iss.str(s), r8.read_att(iss)) {
      (*this) += r8;
    } else if (iss.str(s), rh.read_att(iss)) {
      (*this) += rh;
    } else if (iss.str(s), ymm.read_att(iss)) {
      (*this) += ymm;
    } else if (iss.str(s), xmm.read_att(iss)) {
      (*this) += xmm;
    } else if (iss.str(s), mm.read_att(iss)) {
      (*this) += mm;
    } else if (iss.str(s), ef.read_text(iss)) {
      (*this) += ef;
    } else if (iss.str(s), mxcsr.read_text(iss)) {
      (*this) += mxcsr;
    } else if (iss.str(s), fpustatus.read_text(iss)) {
      (*this) += fpustatus;
    } else if (iss.str(s), fputag.read_text(iss)) {
      (*this) += fputag;
    } else if (iss.str(s), fpucontrol.read_text(iss)) {
      (*this) += fpucontrol;
    } else {
      is.setstate(ios::failbit);
      return is;
    }
  }

  return is;
}

ostream& RegSet::write_text(ostream& os) const {
  os << "{ ";
  for(auto rit = gp_begin(); rit != gp_end(); ++rit) {
    (*rit).write_att(os) << " ";
  }
  for(auto sit = sse_begin(); sit != sse_end(); ++sit) {
    (*sit).write_att(os) << " ";
  }
  for(auto mit = mm_begin(); mit != mm_end(); ++mit) {
    (*mit).write_att(os) << " ";
  }
  for (size_t i = 0, ie = Constants::eflags().size(); i < ie; i += Constants::eflags()[i].width()) {
    if (contains(Constants::eflags()[i])) {
      Constants::eflags()[i].write_text(os) << " ";
    }
  }
  for (size_t i = 0, ie = Constants::fpu_control().size(); i < ie; i += Constants::fpu_control()[i].width()) {
    if (contains(Constants::fpu_control()[i])) {
      Constants::fpu_control()[i].write_text(os) << " ";
    }
  }
  for (size_t i = 0, ie = Constants::mxcsr().size(); i < ie; i += Constants::mxcsr()[i].width()) {
    if (contains(Constants::mxcsr()[i])) {
      Constants::mxcsr()[i].write_text(os) << " ";
    }
  }
  for (size_t i = 0, ie = Constants::fpu_status().size(); i < ie; i += Constants::fpu_status()[i].width()) {
    if (contains(Constants::fpu_status()[i])) {
      Constants::fpu_status()[i].write_text(os) << " ";
    }
  }
  for (size_t i = 0, ie = Constants::fpu_tags().size(); i < ie; i += Constants::fpu_tags()[i].width()) {
    if (contains(Constants::fpu_tags()[i])) {
      Constants::fpu_tags()[i].write_text(os) << " ";
    }
  }
  os << "}";

  return os;
}

} // namespace x64asm
