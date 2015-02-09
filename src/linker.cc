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

namespace x64asm {

void Linker::link(Function& fxn) {
	// Record this function if it requires linking. Remember, the assembler
	// will remove label_rels_ whenever they are resolved. So if anything is
	// left in this map, it must require the linker.
	if (!fxn.label_rels_.empty()) {
		fxns_.push_back(&fxn);
	}

	// Aggregate label_defs_ into a single structure and check for multiple defs 
	for (const auto& l : fxn.label_defs_) {
		const auto itr = label_defs_.find(l.first);
		if (itr != label_defs_.end()) {
			multiple_def_ = true;
			md_symbol_ = itr->first;
			return;
		}
		// Here we store global offsets, rather than function local offsets
		label_defs_.insert(itr, {l.first, (uint64_t)fxn.data()+l.second});
	}
}

void Linker::finish() {
	for (auto fxn : fxns_) {
		for (const auto& l : fxn->label_rels_) {
			const auto pos = l.first;

			const auto itr = label_defs_.find(l.second);
			if (itr == label_defs_.end()) {
				undef_symbol_ = true;
				us_symbol_ = l.second;
				return;
			}

			const auto here = (uint64_t)fxn->data() + pos;
			const auto there = itr->second;
			const auto rel = there - here - 4;

			fxn->emit_long(rel, pos);
		}
	}	
}	

} // x64asm
