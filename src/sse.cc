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

#include "src/sse.h"
#include "src/xmm.h"
#include "src/ymm.h"

#include <cassert>

using namespace std;
using namespace x64asm;


namespace x64asm {

ostream& Sse::write_att(ostream& os) const {
  switch(type()) {
  case Type::XMM_0:
  case Type::XMM:
    return static_cast<const Xmm * const>(this)->write_att(os);
    break;

  case Type::YMM:
    return static_cast<const Ymm * const>(this)->write_att(os);
    break;

  default:
    assert(false);
    return os;
  }
}

} // namespace x64asm
