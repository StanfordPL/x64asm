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

#include "src/flag_set.h"

#include <sstream>
#include <string>

using namespace std;

namespace x64asm {

std::istream& FlagSet::read_text(std::istream& is) {
  *this = FlagSet::empty();
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

    Flag f;
    istringstream iss(s);
    iss >> f;

    if (iss.fail()) {
      is.setstate(ios::failbit);
      return is;
    }
    *this += f;
  }

  return is;
}

std::ostream& FlagSet::write_text(std::ostream& os) const {
  os << "{";
  for (size_t i = 0; i < 64; ++i) {
    uint64_t fi = 0x1ull << i;
    // stop after last flag:
    if (fi > (uint64_t) Flag::RTM) {
      break;
    }
    const auto f = (Flag) fi;
    if (contains(f)) {
      os << " " << f;
    }
  }
  os << " }";
  return os;
}

} // namespace x64asm
