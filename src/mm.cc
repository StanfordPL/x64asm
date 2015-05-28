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

#include "src/mm.h"

#include <array>
#include <cassert>
#include <string>

#include "src/constants.h"

using namespace std;
using namespace x64asm;

namespace {

constexpr array<const char*, 8> mms_() {
  return {
    "%mm0", "%mm1", "%mm2", "%mm3", "%mm4", "%mm5", "%mm6", "%mm7"
  };
}

} // namespace

namespace x64asm {

istream& Mm::read_att(istream& is) {
  string temp;
  is >> temp;

  for (size_t i = 0, ie = mms_().size(); i < ie; ++i) {
    if (temp == mms_()[i]) {
      *this = Constants::mms()[i];
      return is;
    }
  }

  is.setstate(ios::failbit);
  return is;
}

ostream& Mm::write_att(ostream& os) const {
  assert(check());
  return (os << mms_()[val_]);
}

} // namespace x64asm
