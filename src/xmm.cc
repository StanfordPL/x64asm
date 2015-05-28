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

#include "src/xmm.h"

#include <array>
#include <cassert>
#include <string>

#include "src/constants.h"

using namespace std;
using namespace x64asm;

namespace {

constexpr array<const char*, 16> xmms_() {
  return {
    "%xmm0", "%xmm1", "%xmm2", "%xmm3", "%xmm4", "%xmm5", "%xmm6", "%xmm7",
    "%xmm8", "%xmm9", "%xmm10", "%xmm11", "%xmm12", "%xmm13", "%xmm14", "%xmm15"
  };
}

} // namespace

namespace x64asm {

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
  return (os << xmms_()[val_]);
}

} // namespace x64asm
