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

#include "src/code.h"

#include <sstream>

using namespace std;

namespace {

#include "src/att.tab.c"
#include "src/lex.att.c"

} // namespace

namespace x64asm {

istream& Code::read_att(istream& is) {
  stringstream ss;
  ss << is.rdbuf();

  auto buffer = att_scan_string(ss.str().c_str());
  att_switch_to_buffer(buffer);
  attparse(is, *this);
  att_delete_buffer(buffer);

  return is;
}

} // namespace x64asm
