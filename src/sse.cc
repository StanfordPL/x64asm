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

#include <array>
#include <cassert>
#include <string>

#include "src/constants.h"
#include "src/sse.h"

using namespace std;

namespace {

constexpr array<const char*, 16> xmms_() {
  return {
    "%xmm0", "%xmm1", "%xmm2", "%xmm3", "%xmm4", "%xmm5", "%xmm6", "%xmm7",
    "%xmm8", "%xmm9", "%xmm10", "%xmm11", "%xmm12", "%xmm13", "%xmm14", "%xmm15"
  };
}

constexpr array<const char*, 16> ymms_() {
  return {
    "%ymm0", "%ymm1", "%ymm2", "%ymm3", "%ymm4", "%ymm5", "%ymm6", "%ymm7",
    "%ymm8", "%ymm9", "%ymm10", "%ymm11", "%ymm12", "%ymm13", "%ymm14", "%ymm15"
  };
}

} // namespace

namespace x64asm {

ostream& Sse::write_att(ostream& os) const {
  switch(type()) {
  case Type::XMM_0:
  case Type::XMM:
    return static_cast<const Xmm*>(this)->write_att(os);
    break;

  case Type::YMM:
    return static_cast<const Ymm*>(this)->write_att(os);
    break;

  default:
    assert(false);
    return os;
  }
}

istream& Xmm::read_att(istream& is) {
  string temp;
  is >> temp;

  for (size_t i = 0, ie = xmms_().size(); i < ie; ++i) {
    if (temp == xmms_()[i]) {
      *this = Constants::xmms()[i];
      return is;
    }
  }

  is.setstate(ios::failbit);
  return is;
}

ostream& Xmm::write_att(ostream& os) const {
  assert(check());
  return (os << xmms_()[val()]);
}

istream& Ymm::read_att(istream& is) {
  string temp;
  is >> temp;

  for (size_t i = 0, ie = ymms_().size(); i < ie; ++i) {
    if (temp == ymms_()[i]) {
      *this = Constants::ymms()[i];
      return is;
    }
  }

  is.setstate(ios::failbit);
  return is;
}

ostream& Ymm::write_att(ostream& os) const {
  assert(check());
  return (os << ymms_()[val()]);
}

} // namespace x64asm
