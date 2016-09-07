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

#include "src/fail.h"
#include "src/sreg.h"

#include <array>
#include <cassert>
#include <string>

#include "src/constants.h"

using namespace cpputil;
using namespace std;
using namespace x64asm;

namespace {


constexpr array<const char*, 6> sregs_() { return {"es","cs","ss","ds","fs","gs"}; }

} // namespace

namespace x64asm {

istream& Sreg::read_att(istream& is) {
  char c;
  is >> c;

  if(c != '%') {
    fail(is) << "Segment registers must begin with '%'" << endl;
    return is;
  }

  char x, y;
  is >> x >> y;
  stringstream ss;
  ss << x << y;
  string temp = ss.str();

  if (failed(is)) {
    fail(is) << "Not enough characters for segment register" << endl;
    return is;
  }


  for (size_t i = 0, ie = sregs_().size(); i < ie; ++i) {
    if (temp == sregs_()[i]) {
      *this = Constants::sregs()[i];
      return is;
    }
  }

  fail(is) << "No such segment register \"" << temp << "\"" << endl;
  return is;
}

ostream& Sreg::write_att(ostream& os) const {
  assert(check());
  return (os << sregs_()[val_]);
}

} // namespace x64asm
