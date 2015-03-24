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

#ifndef X64ASM_SRC_LABEL_H
#define X64ASM_SRC_LABEL_H

#include <cassert>
#include <iostream>
#include <map>
#include <string>

#include "src/operand.h"

namespace x64asm {

/** A symbolic representation of a Rel32. No Rel8 eqivalent is provided. */
class Label : public Operand {
  /** Needs access to underlying value map */
  friend class Linker;

public:
  /** Creates a new, globally unique label. */
  Label() : Operand(Type::LABEL, init()) {}
  /** Creates a named label. Repeated calls will produce identical results. */
  Label(const std::string& s) : Operand(Type::LABEL, init(s)) {}

  /** Returns true if this label is well-formed. */
  bool check() const {
    return val2label().find(val()) != val2label().end();
  }

  /** Returns the text value of this label. */
  const std::string& get_text() const {
    assert(check());
    return val2label()[val()];
  }

  /** Conversion based on label value. */
  operator uint64_t() const {
    return val();
  }

  /** @todo This method is undefined. */
  std::istream& read_att(std::istream& is) {
    is.setstate(std::ios::failbit);
    return is;
  }
  /** Writes this label to an ostream using at&t syntax. */
  std::ostream& write_att(std::ostream& os) const {
    assert(check());
    return (os << val2label()[val()]);
  }

private:
  /** The next previously unused label value. */
  static uint64_t& next_val() {
		static uint64_t val = 0;
		return val;
	}
  /** Global map from label text to values. */
	static std::map<std::string, uint64_t>& label2val() {
		static std::map<std::string, uint64_t> m;
		return m;
	}	
  /** Global map from label text to values. */
	static std::map<uint64_t, std::string>& val2label() {
		static std::map<uint64_t, std::string> m;
		return m;
	}

  /** Returns the next anonymous value */
  uint64_t init() {
    const auto res = next_val()++;
    std::string s(".__x64asm_L" + std::to_string(val()));
    label2val()[s] = val();
    val2label()[val()] = s;

    return res;
  }
  /** Returns either a new label or the one that corresponds to this string */
  uint64_t init(const std::string& s) {
    auto itr = label2val().find(s);
    auto res = 0;
    if (itr == label2val().end()) {
      res = next_val()++;
      label2val()[s] = val();
      val2label()[val()] = s;
    } else {
      res = itr->second;
    }
    return res;
  }
};

} // namespace x64asm

namespace std {

/** iostream overload. */
inline istream& operator>>(istream& is, x64asm::Label& l) {
  return l.read_att(is);
}
/** iostream overload. */
inline ostream& operator<<(ostream& os, const x64asm::Label& l) {
  return l.write_att(os);
}

} // namespace std

#endif
