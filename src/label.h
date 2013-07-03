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

#include <iostream>
#include <map>
#include <string>

#include "src/operand.h"

namespace x64asm {

/** A symbolic representation of a Rel32. No Rel8 eqivalent is provided. */
class Label : public Operand {
  public:
    /** Creates a new, globally unique label. */
    Label();
    /** Creates a named label. Repeated calls will produce identical results. */
    Label(const std::string& s);

    /** Copy constructor. */
    Label(const Label& rhs);
    /** Move constructor. */
    Label(Label&& rhs);
    /** Copy assignment operator. */
    Label& operator=(const Label& rhs);
    /** Move assignment operator. */
    Label& operator=(Label&& rhs);

    /** Returns true if this label is well-formed. */
    bool check() const;

    /** Comparison based on label id. */
    bool operator<(const Label& rhs) const;
    /** Comparison based on label id. */
    bool operator==(const Label& rhs) const;
    /** Comparison based on label id. */
    bool operator!=(const Label& rhs) const;

    /** Conversion based on label value. */
    operator uint64_t() const;

    /** STL-compliant hash. */
    size_t hash() const;
    /** STL-compliant swap. */
    void swap(Label& rhs);

    /** Writes this label to an ostream using at&t syntax. */
    std::ostream& write_att(std::ostream& os) const;

  private:
    /** Global map of named labels. */
    static std::map<std::string, uint64_t> labels_;
    /** The next previously unused label value. */
    static uint64_t next_val_;
};

} // namespace x64asm

namespace std {

/** STL hash specialization. */
template <>
struct hash<x64asm::Label> {
  size_t operator()(const x64asm::Label& l) const;
};

/** STL swap overload. */
void swap(x64asm::Label& lhs, x64asm::Label& rhs);

/** I/O overload. */
ostream& operator<<(ostream& os, const x64asm::Label& l);

} // namespace std

namespace x64asm {

inline Label::Label() {
  val_ = next_val_++;
}

inline Label::Label(const std::string& s) {
  val_ = labels_.find(s) == labels_.end() ?
    labels_.insert(std::make_pair(s, next_val_++)).first->second :
    labels_.find(s)->second;
}

inline Label::Label(const Label& rhs) {
  val_ = rhs.val_;
}

inline Label::Label(Label&& rhs) {
  val_ = rhs.val_;
}

inline Label& Label::operator=(const Label& rhs) {
  Label(rhs).swap(*this);
  return *this;
}

inline Label& Label::operator=(Label&& rhs) {
  Label(std::move(rhs)).swap(*this);
  return *this;
}

inline bool Label::check() const {
  return true;
}

inline bool Label::operator<(const Label& rhs) const {
  return val_ < rhs.val_;
}

inline bool Label::operator==(const Label& rhs) const {
  return val_ == val_;
}

inline bool Label::operator!=(const Label& rhs) const {
  return !(*this == rhs);
}

inline Label::operator uint64_t() const {
  return val_;
}

inline size_t Label::hash() const {
  return val_;
}

inline void Label::swap(Label& rhs) {
  std::swap(val_, rhs.val_);
}

inline std::ostream& Label::write_att(std::ostream& os) const {
  return (os << ".L_X64ASM_" << std::dec << val_);
}

} // namespace x64asm 

namespace std {

inline size_t hash<x64asm::Label>::operator()(const x64asm::Label& l) const {
  return l.hash();
}

inline void swap(x64asm::Label& lhs, x64asm::Label& rhs) {
  lhs.swap(rhs);
}

inline ostream& operator<<(ostream& os, const x64asm::Label& x) {
  return x.write_att(os);
}

} // namespace std

#endif
