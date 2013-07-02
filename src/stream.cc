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

} // namespace

