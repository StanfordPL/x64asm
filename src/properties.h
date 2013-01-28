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

#ifndef X64ASM_SRC_PROPERTIES_H
#define X64ASM_SRC_PROPERTIES_H

#include <stdint.h>

namespace x64asm {

enum class Property : uint32_t {
	MUST_READ      = 0x00000003,
	MAYBE_READ     = 0x00000001,
	NO_READ        = 0x00000010,

	MUST_WRITE_ZX  = 0x00000700,
	MUST_WRITE     = 0x00000300,
	MAYBE_WRITE_ZX = 0x00000500,
	MAYBE_WRITE    = 0x00000100,
	NO_WRITE       = 0x00001000,

	MUST_UNDEF     = 0x00030000,
	MAYBE_UNDEF    = 0x00010000,
	NO_UNDEF       = 0x00100000
};

class Properties {
	private:
		inline Properties(Property r, Property w, Property u) 
				: mask_{(uint32_t)r | (uint32_t)w | (uint32_t)u} {
		}

	public:
		// Static Constants
		static inline Properties none() {
			return Properties(Property::NO_READ, Property::NO_WRITE, 
					              Property::NO_UNDEF);
		}

		// Element Operators
		inline Properties operator+(Property rhs) const {
			auto ret = *this;
			return ret += rhs;
		}

		inline Properties operator-(Property rhs) const {
			auto ret = *this;
			return ret -= rhs;
		}

		inline Properties& operator+=(Property rhs) {
			mask_ |= ((uint32_t)rhs);
			return *this;
		}

		inline Properties& operator-=(Property rhs) {
			mask_ &= ~((uint32_t)rhs);
			return *this;
		}

		// Queries
		inline bool contains(Property p) const {
			return (mask_ & (uint32_t)p) == (uint32_t)p;
		}

	private:
		uint32_t mask_;
};

} // namespace x64asm

#endif
