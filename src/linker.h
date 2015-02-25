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

#ifndef X64ASM_SRC_LINKER_H
#define X64ASM_SRC_LINKER_H

#include <unordered_map>
#include <vector>

#include "src/function.h"
#include "src/label.h"

namespace x64asm {

class Linker {
public:
  /** Restart the linking process */
  void start() {
    multiple_def_ = false;
    undef_symbol_ = false;
    label_defs_.clear();
    fxns_.clear();
  }
  /** Link a new function */
  void link(Function& fxn);
  /** Finish the linking process */
  void finish();

  /** Returns true if no errors occurred during linking. */
  bool good() const {
    return !multiple_def() && !undef_symbol();
  }
  /** Returns true if a multiple definition error occurred. */
  bool multiple_def() const {
    return multiple_def_;
  }
  /** Returns the symbol that caused a multiple definition error. */
  Label get_multiple_def() const {
    assert(multiple_def());
    return Label::val2label_[md_symbol_];
  }
  /** Returns true if an undefined symbol error occurred. */
  bool undef_symbol() const {
    return undef_symbol_;
  }
  /** Returns the symbol that caused an undefined symbol error. */
  Label get_undef_symbol() const {
    assert(undef_symbol());
    return Label::val2label_[us_symbol_];
  }

private:
  /** Label definition map for all functions (uses global addrs). */
  std::unordered_map<uint64_t, size_t> label_defs_;
  /** List of functions that require linking. */
  std::vector<Function*> fxns_;
  /** Did a multiple definition error occur? */
  bool multiple_def_;
  /** What was the name of this symbol? */
  uint64_t md_symbol_;
  /** Did an undefined symbol error occur? */
  bool undef_symbol_;
  /** What was the name of this symbol? */
  uint64_t us_symbol_;
};

} // namespace x64asm

#endif
