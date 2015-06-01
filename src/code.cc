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
#include "src/fail.h"

#include <sstream>
#include <iostream>

using namespace std;
using namespace cpputil;

namespace x64asm {

istream& Code::read_att(istream& is) {

  clear();

  size_t line_no = 0;
  string line;
  bool failed = false;
  stringstream failss;
  while(is.good()) {
    line_no++;

    getline(is, line);

    // check for empty lines
    if(!line.size())
      continue;

    bool nonempty = false;
    size_t comment = line.find_first_of('#');
    if(comment != string::npos) {
      for(size_t i = 0; i < comment; ++i) {
        if(line[i] != ' ')
          nonempty = true;
      }
      if(!nonempty)
        continue;
    }

    // parse an instruction
    stringstream line_ss(line);
    auto instr = Instruction(NOP);
    line_ss >> instr;

    if (!line_ss.fail()) {
      push_back(instr);
    } else {
      failed = true;
      failss << "Failed to parse line " << line_no << ": " << line << endl;
      failss << "Message: " << fail_msg(line_ss) << endl;
    }
  }

  if(failed || is.bad())
    fail(is) << failss.str();
  else 
    is.clear();

  return is;
}

ostream& Code::write_att(ostream& os) const {
  for (size_t i = 0, ie = size(); i < ie; ++i) {
    (*this)[i].write_att(os);
    if (i+1 != ie) {
      os << std::endl;
    }
  }
  return os;
}

} // namespace x64asm
