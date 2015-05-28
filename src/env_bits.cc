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

#include "src/env_bits.h"

#include <array>
#include <string>

#include "src/constants.h"

using namespace std;
using namespace x64asm;

namespace {

constexpr array<const char*, 22> eflags_() {
  return {
    "%cf", "<res1>", "%pf", "<res3>", "%af", "<res5>", "%zf", "%sf",
    "%tf", "%if", "%df", "%of", "%iopl", "%iopl", "%nt", "<res15>",
    "%rf", "%vm", "%ac", "%vif", "%vip", "%id"
  };
}

constexpr array<const char*, 16> control_() {
  return {
    "%control::im", "%control::dm", "%control::zm", "%control::om",
    "%control::um", "%control::pm", "<control::res6>", "<control::res7>",
    "%control::pc[0]", "%control::pc[1]", "%control::rc[0]", "%control::rc[1]",
    "%control::x", "<control::res13>", "<control::res14>", "<control::res15>"
  };
}

constexpr array<const char*, 16> status_() {
  return {
    "%status::ie", "%status::de", "%status::ze", "%status::oe",
    "%status::ue", "%status::pe", "%status::sf", "%status::es",
    "%status::c0", "%status::c1", "%status::c2", "%status::top[0]",
    "%status::top[1]", "%status::top[2]", "%status::c3", "%status::b"
  };
}

constexpr array<const char*, 16> tag_() {
  return {
    "%tag0", "%tag0", "%tag1", "%tag1", "%tag2", "%tag2", "%tag3", "%tag3",
    "%tag4", "%tag4", "%tag5", "%tag5", "%tag6", "%tag6", "%tag7", "%tag7"
  };
}

constexpr array<const char*, 16> mxcsr_() {
  return {
    "%mxcsr::ie", "%mxcsr::de", "%mxcsr::ze", "%mxcsr::oe",
    "%mxcsr::ue", "%mxcsr::pe", "%mxcsr::daz", "%mxcsr::im",
    "%mxcsr::dm", "%mxcsr::zm", "%mxcsr::om", "%mxcsr::um",
    "%mxcsr::pm", "%mxcsr::rc[0]", "%mxcsr::rc[1]", "%mxcsr::fz"
  };
}

} // namespace

namespace x64asm {

istream& Eflags::read_text(istream& is) {
  string temp;
  is >> temp;

  for (size_t i = 0, ie = eflags_().size(); i < ie; ++i) {
    if (temp == eflags_()[i]) {
      *this = Constants::eflags()[i];
      return is;
    }
  }

  is.setstate(ios::failbit);
  return is;
}

ostream& Eflags::write_text(ostream& os) const {
  if (index() >= eflags_().size()) {
    os.setstate(ios::failbit);
    return os;
  }
  os << eflags_()[index()];
  return os;
}

istream& FpuControl::read_text(istream& is) {
  string temp;
  is >> temp;

  for (size_t i = 0, ie = control_().size(); i < ie; ++i) {
    if (temp == control_()[i]) {
      *this = Constants::fpu_control()[i];
      return is;
    }
  }

  is.setstate(ios::failbit);
  return is;
}

ostream& FpuControl::write_text(ostream& os) const {
  if (index() >= control_().size()) {
    os.setstate(ios::failbit);
    return os;
  }
  os << control_()[index()];
  return os;
}

istream& FpuStatus::read_text(istream& is) {
  string temp;
  is >> temp;

  for (size_t i = 0, ie = status_().size(); i < ie; ++i) {
    if (temp == status_()[i]) {
      *this = Constants::fpu_status()[i];
      return is;
    }
  }

  is.setstate(ios::failbit);
  return is;
}

ostream& FpuStatus::write_text(ostream& os) const {
  if (index() >= status_().size()) {
    os.setstate(ios::failbit);
    return os;
  }
  os << status_()[index()];
  return os;
}

istream& FpuTag::read_text(istream& is) {
  string temp;
  is >> temp;

  for (size_t i = 0, ie = tag_().size(); i < ie; ++i) {
    if (temp == tag_()[i]) {
      *this = Constants::fpu_tags()[i];
      return is;
    }
  }

  is.setstate(ios::failbit);
  return is;
}

ostream& FpuTag::write_text(ostream& os) const {
  if (index() >= tag_().size()) {
    os.setstate(ios::failbit);
    return os;
  }
  os << tag_()[index()];
  return os;
}

istream& Mxcsr::read_text(istream& is) {
  string temp;
  is >> temp;

  for (size_t i = 0, ie = mxcsr_().size(); i < ie; ++i) {
    if (temp == mxcsr_()[i]) {
      *this = Constants::mxcsr()[i];
      return is;
    }
  }

  is.setstate(ios::failbit);
  return is;
}

ostream& Mxcsr::write_text(ostream& os) const {
  if (index() >= mxcsr_().size()) {
    os.setstate(ios::failbit);
    return os;
  }
  os << mxcsr_()[index()];
  return os;
}

} // namespace x64asm
