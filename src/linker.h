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

#include <vector>

#include "src/function.h"

namespace x64asm {

class Linker {
	public:
		/** Restart the linking process */
		void start() {
			fxns_.clear();
		}

		/** Link a new function */
		void link(Function& fxn) {
			fxns_.push_back(&fxn);
		}

		/** Finish the linking process */
		void finish() {
			for (auto f1 : fxns_) {
				for (const auto& l : f1->label_rels_) {
					for (auto f2 : fxns_) {
						if (f1 == f2) {
							continue;
						}

						const auto pos = l.first;
						const auto itr = f2->label_defs_.find(l.second);

						if (itr != f2->label_defs_.end()) {
							const auto offset = (uint64_t)f2->data() - (uint64_t)f1->data();
							const auto rel = offset + itr->second - pos - 4;
							f1->emit_long(rel, pos);
						}
					}
				}
			}	
		}	

	private:
		std::vector<Function*> fxns_;
};

} // namespace x64asm

#endif
