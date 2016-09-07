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

#include <ios>
#include <sstream>

#include "src/alias.h"
#include "src/fail.h"

using namespace std;
using namespace cpputil;

namespace x64asm {

bool Mem::check() const {
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
  if (contains_index() && get_index().val_ == Constants::esp().val_) {
    return false;
  }
  // Check for absence of base/index for RIP+offset form
  if (rip_offset() && (contains_base() || contains_index())) {
    return false;
  }

  return true;
}

std::istream& Mem::read_att(std::istream& is) {

  bool ok = false;
  char tmp;

  // Segment register
  if(is.peek() == '%') {
    Sreg sreg = ss;
    sreg.read_att(is);
    set_seg(sreg);

    if(cpputil::failed(is)) {
      return is;
    }


    if(is.get() != ':') {
      cpputil::fail(is) << "Tried to parse segment register as memory, but no ':' found.";
      return is;
    }
  } else {
    clear_seg();
  }

  // Displacement
  bool neg = false;
  uint64_t disp = 0;
  if(is.peek() == '-') {
    neg = true;
    is.ignore();
  }
  tmp = is.peek();
  if(tmp == '0') {
    ok = true;
    is.ignore();
    tmp = is.peek();
    if(tmp == 'x') {
      is.ignore();
      is >> std::hex >> disp;
    } else if ('0' <= tmp && tmp <= '9') {
      is >> std::dec >> disp;
    } else {
      disp = 0;
    }
  } else if ('1' <= tmp && tmp <= '9') {
    ok = true;
    is >> std::dec >> disp; 
  }
  if(neg)
    disp = -disp;
  set_disp(Imm32(disp & 0xffffffff));

  // base/index/scale?
  if(is.peek() == '(') {
    ok = true;
    is.ignore();
    is >> std::ws;

    // Base
    if(is.peek() != ',') {

      std::stringstream name;
      for(char tmp = is.peek(); ('a' <= tmp && tmp <= 'z') || ('0' <= tmp && tmp <= '9') || tmp == '%'; tmp = is.peek()) {
        is.ignore();
        name.put(tmp);
      }

      if(name.str() == "%rip") {
        clear_base();
        set_rip_offset(true);
      } else {
        R base = rax;
        base.read_att(name);
        if(cpputil::failed(name)) {
          cpputil::fail(is) << cpputil::fail_msg(name);
          return is;
        }

        set_base(base);
        set_rip_offset(false);
        if(base.size() == 32)
          set_addr_or(true);
        else if(base.size() == 64)
          set_addr_or(false);
        else {
          cpputil::fail(is) << "Base (" << base << ") "
                            << "of memory reference must be 32 or 64 bit register." << std::endl;
          return is;
        }
      }
    } else {
      clear_base();
      set_rip_offset(false);
    }

    // Index
    is >> std::ws;
    if(is.peek() == ',') {
      is.ignore();
      is >> std::ws;

      std::stringstream name;
      for(char tmp = is.peek(); ('a' <= tmp && tmp <= 'z') || ('0' <= tmp && tmp <= '9') || tmp == '%'; tmp = is.peek()) {
        is.ignore();
        name.put(tmp);
      }

      R index = rax;
      index.read_att(name);

      if(cpputil::failed(name)) {
        cpputil::fail(is) << cpputil::fail_msg(name);
        return is;
      }

      if(contains_base()) {
        if((addr_or() && index.size() != 32) ||
            (!addr_or() && index.size() != 64)) {
          cpputil::fail(is) << "Base (" << get_base() << ") and index (" << index 
                            << ") registers must have same width." << std::endl;
          return is;
        }
      } else {
        if(index.size() == 32)
          set_addr_or(true);
        else if(index.size() == 64)
          set_addr_or(false);
        else {
          cpputil::fail(is) << "Index (" << index << ") "
                            << "of memory reference must be 32 or 64 bit register." << std::endl;
        }
      }

      set_index(index);
      // Scale
      set_scale(Scale::TIMES_1);
      is >> std::ws;
      if(is.peek() == ',')  {
        is.ignore();
        size_t n = 0;
        is >> n;
        switch(n) {
          case 1:
            set_scale(Scale::TIMES_1);
            break;
          case 2:
            set_scale(Scale::TIMES_2);
            break;
          case 4:
            set_scale(Scale::TIMES_4);
            break;
          case 8:
            set_scale(Scale::TIMES_8);
            break;
          default:
            cpputil::fail(is) << "Scale on memory reference must be 1, 2, 4, or 8";
        }
      }
    } else {
      clear_index();
    }
    is >> std::ws;

    if(is.get() != ')') {
      cpputil::fail(is) << "Expected ')' at end of memory reference";
      return is;
    }

  } else {
    clear_base();
    clear_index();
    set_rip_offset(false);
  }

  if(!ok) {  //it doesn't look like a memory reference.  could be anything.
    cpputil::fail(is) << "Don't know how to parse this!";
  }
  return is;
}


std::ostream& Mem::write_att(std::ostream& os) const {
  if (contains_seg()) {
    get_seg().write_att(os);
    os << ":";
  }
  if ((uint64_t)get_disp() != 0 || (!contains_base() && !contains_index())) {
    const auto d = (int32_t)(get_disp() & 0x00000000ffffffff);
    const auto fmt = os.flags();
    if (d < 0) {
      os << "-0x" << std::noshowbase << std::hex << -d;
    } else {
      os << "0x" << std::noshowbase << std::hex << d;
    }
    os.flags(fmt);
  }
  if (!contains_base() && !contains_index() && !rip_offset()) {
    return os;
  }
  os << "(";
  if (rip_offset()) {
    os << "%rip";
  }
  if (contains_base()) {
    const auto b = get_base();
    if (addr_or()) {
      Alias::to_double(b).write_att(os);
    } else {
      b.write_att(os);
    }
  }
  if (contains_index()) {
    os << ",";
  }
  if (contains_index()) {
    const auto i = get_index();
    if (addr_or()) {
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

  return os;
}

} // namespace x64asm
