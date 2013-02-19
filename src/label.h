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
		Label() {
			val_ = next_val_++;
		}

		Label(const std::string& s) {
			val_ = labels_.find(s) == labels_.end() ?
			  labels_.insert(std::make_pair(s, next_val_++)).first->second :
			  labels_.find(s)->second;
		}

		bool check() const {
			return true;
		}

		uint64_t val() const {
			return val_;
		}

		bool operator<(const Label& rhs) const {
			return val_ < rhs.val_;
		}

		bool operator==(const Label& rhs) const {
			return val_ == val_;
		}

		operator uint64_t() const {
			return val_;
		}

		void write_att(std::ostream& os) const;
		void write_intel(std::ostream& os) const;

	private:
		static std::map<std::string, uint64_t> labels_;	
		static uint64_t next_val_;
};

} // namespace x64asm

#endif
