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

#include "src/ymm.h"

#include <array>
#include <cassert>
#include <string>

#include "src/constants.h"

using namespace std;
using namespace x64asm;

namespace {

constexpr array<const char*, 16> ymms_() {
  return {
    "%ymm0", "%ymm1", "%ymm2", "%ymm3", "%ymm4", "%ymm5", "%ymm6", "%ymm7",
    "%ymm8", "%ymm9", "%ymm10", "%ymm11", "%ymm12", "%ymm13", "%ymm14", "%ymm15"
  };
}

} // namespace

namespace x64asm {

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
  return (os << ymms_()[val_]);
}

} // namespace x64asm
