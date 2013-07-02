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

#include "src/stream.h"

#include "src/assembler.h"

using namespace std;
using namespace x64asm;

namespace {

template <typename T>
inline void check(ostream& os, const T& t) {
  if (!t.check()) {
    os.setstate(ios::failbit);
  }
}

template <typename T>
ostream& write(ostream& os, const T& t) {
  t.write_att(os);
  return os;
}

} // namespace

istream& operator>>(istream& is, Code& c) {
  c.read_att(is);
  return is;
}

ostream& operator<<(ostream& os, const Code& c) {
  check(os, c);
  return write(os, c);
}

ostream& operator<<(ostream& os, const Instruction& i) {
  check(os, i);
  return write(os, i);
}

ostream& operator<<(ostream& os, const M8& m) {
  check(os, m);
  return write(os, m);
}

ostream& operator<<(ostream& os, const M16& m) {
  check(os, m);
  return write(os, m);
}

ostream& operator<<(ostream& os, const M32& m) {
  check(os, m);
  return write(os, m);
}

ostream& operator<<(ostream& os, const M64& m) {
  check(os, m);
  return write(os, m);
}

ostream& operator<<(ostream& os, const M128& m) {
  check(os, m);
  return write(os, m);
}

ostream& operator<<(ostream& os, const M256& m) {
  check(os, m);
  return write(os, m);
}

ostream& operator<<(ostream& os, const M16Int& m) {
  check(os, m);
  return write(os, m);
}

ostream& operator<<(ostream& os, const M32Int& m) {
  check(os, m);
  return write(os, m);
}

ostream& operator<<(ostream& os, const M64Int& m) {
  check(os, m);
  return write(os, m);
}

ostream& operator<<(ostream& os, const M32Fp& m) {
  check(os, m);
  return write(os, m);
}

ostream& operator<<(ostream& os, const M64Fp& m) {
  check(os, m);
  return write(os, m);
}

ostream& operator<<(ostream& os, const M80Fp& m) {
  check(os, m);
  return write(os, m);
}

ostream& operator<<(ostream& os, const M80Bcd& m) {
  check(os, m);
  return write(os, m);
}

ostream& operator<<(ostream& os, const FarPtr1616& m) {
  check(os, m);
  return write(os, m);
}

ostream& operator<<(ostream& os, const FarPtr1632& m) {
  check(os, m);
  return write(os, m);
}

ostream& operator<<(ostream& os, const FarPtr1664& m) {
  check(os, m);
  return write(os, m);
}

ostream& operator<<(ostream& os, const Pref66& p) {
  check(os, p);
  return write(os, p);
}

ostream& operator<<(ostream& os, const PrefRexW& p) {
  check(os, p);
  return write(os, p);
}

ostream& operator<<(ostream& os, const Far& f) {
  check(os, f);
  return write(os, f);
}

ostream& operator<<(ostream& os, const Moffs8& m) {
  check(os, m);
  return write(os, m);
}

ostream& operator<<(ostream& os, const Moffs16& m) {
  check(os, m);
  return write(os, m);
}

ostream& operator<<(ostream& os, const Moffs32& m) {
  check(os, m);
  return write(os, m);
}

ostream& operator<<(ostream& os, const Moffs64& m) {
  check(os, m);
  return write(os, m);
}

ostream& operator<<(ostream& os, const Rel8& r) {
  check(os, r);
  return write(os, r);
}

ostream& operator<<(ostream& os, const Rel32& r) {
  check(os, r);
  return write(os, r);
}

ostream& operator<<(ostream& os, const Sreg& s) {
  check(os, s);
  return write(os, s);
}

ostream& operator<<(ostream& os, const Fs& s) {
  check(os, s);
  return write(os, s);
}

ostream& operator<<(ostream& os, const Gs& s) {
  check(os, s);
  return write(os, s);
}

ostream& operator<<(ostream& os, const St& s) {
  check(os, s);
  return write(os, s);
}

ostream& operator<<(ostream& os, const St0& s) {
  check(os, s);
  return write(os, s);
}
