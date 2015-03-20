/*
Copyright 2014 eric schkufza

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

#include "src/flag.h"

#include <array>
#include <cassert>
#include <string>
#include <utility>

using namespace std;
using namespace x64asm;

namespace {

constexpr array<pair<const char*, Flag>, 41> flags_() {
  return {{
    {"fpu", Flag::FPU},
    {"tsc", Flag::TSC},
    {"msr", Flag::MSR},
    {"cx8", Flag::CX8},
    {"sep", Flag::SEP},
    {"cmov", Flag::CMOV},
    {"clflush", Flag::CLFLUSH},
    {"mmx", Flag::MMX},
    {"fxsr", Flag::FXSR},
    {"sse", Flag::SSE},
    {"sse2", Flag::SSE2},
    {"syscall", Flag::SYSCALL},
    {"rdtscp", Flag::RDTSCP},
    {"rep_good", Flag::REP_GOOD},
    {"nopl", Flag::NOPL},
    {"pni", Flag::PNI},
    {"pclmulqdq", Flag::PCLMULQDQ},
    {"monitor", Flag::MONITOR},
    {"ssse3", Flag::SSSE3},
    {"fma", Flag::FMA},
    {"cx16", Flag::CX16},
    {"sse4_1", Flag::SSE4_1},
    {"sse4_2", Flag::SSE4_2},
    {"movbe", Flag::MOVBE},
    {"popcnt", Flag::POPCNT},
    {"aes", Flag::AES},
    {"xsave", Flag::XSAVE},
    {"avx", Flag::AVX},
    {"f16c", Flag::F16C},
    {"rdrand", Flag::RDRAND},
    {"lahf_lm", Flag::LAHF_LM},
    {"abm", Flag::ABM},
    {"xsaveopt", Flag::XSAVEOPT},
    {"fsgsbase", Flag::FSGSBASE},
    {"bmi1", Flag::BMI1},
    {"hle", Flag::HLE},
    {"avx2", Flag::AVX2},
    {"bmi2", Flag::BMI2},
    {"erms", Flag::ERMS},
    {"invpcid", Flag::INVPCID},
    {"rtm", Flag::RTM}
  }};
}

} // namespace

namespace x64asm {

istream& read_text(istream& is, Flag& f) {
  string temp;
  is >> temp;

  for (const auto& p : flags_()) {
    if (temp == p.first) {
      f = p.second;
      return is;
    }
  }

  is.setstate(ios::failbit);
  return is;
}

ostream& write_text(ostream& os, const Flag f) {
  for (const auto& p : flags_()) {
    if (f == p.second) {
      os << p.first;
      return os;
    }
  }

  os.setstate(ios::failbit);
  return os;
}

} // namespace x64asm
