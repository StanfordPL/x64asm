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

#include "src/linker.h"

using namespace std;

#define DEBUG_LINKER(X) { }

namespace x64asm {

void Linker::link(Function& fxn, uint64_t offset) {
  // Record this function if it requires linking. Remember, the assembler
  // will remove label_rels_ whenever they are resolved. So if anything is
  // left in this map, it must require the linker.
  if (!fxn.label8_rels_.empty() || !fxn.label32_rels_.empty()) {
    fxns_.insert(fxns_.begin(), {&fxn, offset});
  }

  // Save offsets for each label and check for multiple defs
  for (const auto& l : fxn.label_defs_) {
    link(l.first, offset + l.second);
  }
}

void Linker::link(uint64_t symbol, uint64_t address) {
  // Check for multiple defs
  const auto itr = label_defs_.find(symbol);
  if (itr != label_defs_.end()) {
    multiple_def_ = true;
    md_symbol_ = itr->first;
    DEBUG_LINKER(cout << "[linker] mulitple definition error: " << Label::val2label()[md_symbol_] << endl;)
    return;
  }

  // Here we store global offsets, rather than function local offsets
  label_defs_.insert(itr, {symbol, address});
}

void Linker::finish() {
  for (auto fxn_offset_pair : fxns_) {
    auto fxn = fxn_offset_pair.first;

    for (const auto& l : fxn->label32_rels_) {
      const auto pos = l.first;

      const auto itr_target = label_defs_.find(l.second);
      if (itr_target == label_defs_.end()) {
        undef_symbol_ = true;
        us_symbol_ = l.second;
        DEBUG_LINKER(cout << "[linker] undef symbol error: " << undef_symbol_ << endl;)
        return;
      }

      const auto here = fxn_offset_pair.second + pos;
      const auto there = itr_target->second;
      const auto rel = there - here - 4;


      if(rel > 0x7fffffff && rel < 0xffffffff80000000) {
        cout << "rel = " << hex << rel << endl;
        jump_too_far_ = true;
        DEBUG_LINKER(cout << "[linker] jump too far error" << endl;)
        return;
      }

      fxn->emit_long(rel, pos);
    }

    for (const auto& l : fxn->label8_rels_) {
      const auto pos = l.first;

      const auto itr = label_defs_.find(l.second);
      if (itr == label_defs_.end()) {
        undef_symbol_ = true;
        us_symbol_ = l.second;
        DEBUG_LINKER(cout << "[linker] undef symbol error: " << undef_symbol_ << endl;)
        return;
      }

      const auto here = (uint64_t)fxn_offset_pair.second + pos;
      const auto there = itr->second;
      const auto rel = there - here - 1;

      if(rel > 0x7f && rel < 0xffffffffffffff80) {
        jump_too_far_ = true;
        DEBUG_LINKER(cout << "[linker] jump too far error" << endl;)
        return;
      }
      fxn->emit_byte(rel, pos);
    }
  }
}

} // x64asm
