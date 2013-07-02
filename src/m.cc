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

#include "src/alias.h"
#include "src/m.h"

using namespace std;

namespace x64asm {

bool M::check() const {
  // Check seg
  if (contains_seg() && !get_seg().check()) {
    return false;
  }
  // Check base
  if (contains_base() && !get_base().check()) {
    return false;
  }
  // Check index
  if (contains_index() && !get_index().check()) {
    return false;
  }
  // Check scale
  switch (get_scale()) {
    case Scale::TIMES_1:
    case Scale::TIMES_2:
    case Scale::TIMES_4:
    case Scale::TIMES_8:
      break;
    default:
      return false;
  }
  // Check disp
  if (!get_disp().check()) {
    return false;
  }

  // Index cannot be rsp/esp
  if (contains_index() && get_index().val_ == esp.val_) {
    return false;
  }

  // Check for absence of base/index for RIP+offset form
  if (rip_offset() && (contains_base() || contains_index())) {
    return false;
  }

  return true;
}

void M::write_att(ostream& os) const {
  if (contains_seg()) {
    get_seg().write_att(os);
    os << ":";
  }
  if (get_disp().val_ != 0) {
    os << "0x" << noshowbase << hex << get_disp().val_;
  }

  if (!contains_base() && !contains_index() && !rip_offset()) {
    return;
  }

  os << "(";
  if (rip_offset()) {
    os << "%rip";
  }
  if (contains_base()) {
    const auto b = get_base();
    if (get_addr_or()) {
      Alias::to_double(b).write_att(os);
    } else {
      b.write_att(os);
    }
  }
  if (contains_base() && contains_index()) {
    os << ",";
  }
  if (contains_index()) {
    const auto i = get_index();
    if (get_addr_or()) {
      Alias::to_double(i).write_att(os);
    } else {
      i.write_att(os);
    }
    os << ",";
    switch (get_scale()) {
      case Scale::TIMES_1:
        os << "1";
        break;
      case Scale::TIMES_2:
        os << "2";
        break;
      case Scale::TIMES_4:
        os << "4";
        break;
      case Scale::TIMES_8:
        os << "8";
        break;
      default:
        assert(false);
    }
  }
  os << ")";
}

} // namespace x64asm
