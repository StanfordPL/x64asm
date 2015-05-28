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

#include "src/code.h"

#include <sstream>
#include <iostream>

using namespace std;

namespace {

//#include "src/att.tab.c"
//#include "src/lex.att.c"

} // namespace

namespace x64asm {

istream& Code::read_att(istream& is) {

  clear();

  size_t line_no = 0;
  string line;
  while(is.good()) {
    line_no++;

    getline(is, line);
    stringstream line_ss(line);

    auto instr = Instruction(NOP);
    line_ss >> instr;

    if (line_ss.good() || line_ss.eof()) {
      push_back(instr);
    } else {
      cerr << "Failed to parse line " << line_no << ": " << line << endl;
    }
  }

  if(is.eof())
    is.clear();

  return is;
}

} // namespace x64asm
