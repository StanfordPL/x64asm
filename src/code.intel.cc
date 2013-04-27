/*
Copyright 2103 eric schkufza

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

#include "src/code.h"

#include <sstream>

using namespace std;

namespace {

#include "src/intel.tab.c"
#include "src/lex.intel.c"

} // namespace

namespace x64asm {

void Code::read_intel(istream& is) {
  stringstream ss;
  ss << is.rdbuf();

  auto buffer = intel_scan_string(ss.str().c_str());
  intel_switch_to_buffer(buffer);
  intelparse(is, *this);
  intel_delete_buffer(buffer);
}

} // namespace x64asm

