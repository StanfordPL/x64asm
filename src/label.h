/*
Copyright 2103 eric schkufza

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

/** A symbolic representation of a Rel32.
	  For simplicity, we do not provide a Rel8 equivalent.
*/
class Label : public Operand {
	public:
		constexpr Label(uint64_t val) 
				: Operand{val} { 
		}

		constexpr bool check() {
			return true;
		}

		void write_att(std::ostream& os) const;
		void write_intel(std::ostream& os) const;

		static Label get(const std::string& s) {
			const auto itr = labels_.find(s);
			if ( itr == labels_.end() ) {
				const auto elem = std::make_pair(s, Label{labels_.size()});
				return labels_.insert(elem).first->second;
			}
			return itr->second;
		}

	private:
		static std::map<std::string, Label> labels_;	
};

} // namespace x64asm

#endif
