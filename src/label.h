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
  Label() : Operand(Type::LABEL) {
    val_ = next_val()++;
    std::string s(".__x64asm_L" + std::to_string(val_));
    label2val()[s] = val_;
    val2label()[val_] = s;
  }
  /** Creates a named label. Repeated calls will produce identical results. */
  Label(const std::string& s) : Operand(Type::LABEL) {
    auto itr = label2val().find(s);
    if (itr == label2val().end()) {
      val_ = next_val()++;
      label2val()[s] = val_;
      val2label()[val_] = s;
    } else {
      val_ = itr->second;
    }
  }

  /** Returns true if this label is well-formed. */
  bool check() const {
    return val2label().find(val_) != val2label().end();
  }

  /** Returns the text value of this label. */
  const std::string& get_text() const {
    assert(check());
    return val2label()[val_];
  }

  /** Comparison based on label id. */
  bool operator<(const Label& rhs) const {
    return val_ < rhs.val_;
  }
  /** Comparison based on label id. */
  bool operator==(const Label& rhs) const {
    return val_ == rhs.val_;
  }
  /** Comparison based on label id. */
  bool operator!=(const Label& rhs) const {
    return !(*this == rhs);
  }

  /** Conversion based on label value. */
  operator uint64_t() const {
    return val_;
  }

  /** STL-compliant hash. */
  size_t hash() const {
    return val_;
  }
  /** @todo This method is undefined. */
  std::istream& read_att(std::istream& is) {
    is.setstate(std::ios::failbit);
    return is;
  }
  /** Writes this label to an ostream using at&t syntax. */
  std::ostream& write_att(std::ostream& os) const {
    assert(check());
    return (os << val2label()[val_]);
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
};

} // namespace x64asm

namespace std {

/** STL hash specialization. */
template <>
struct hash<x64asm::Label> {
  size_t operator()(const x64asm::Label& l) const {
    return l.hash();
  }
};

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
