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

	MUST_WRITE_ZX  = 0x00000700,
	MUST_WRITE     = 0x00000300,
	MAYBE_WRITE_ZX = 0x00000500,
	MAYBE_WRITE    = 0x00000100,

	MUST_UNDEF     = 0x00030000,
	MAYBE_UNDEF    = 0x00010000,

	NONE           = 0x00000000,
	ALL            = 0x00030703
};

class Properties {
	private:
		constexpr Properties(uint32_t p) 
				: mask_{p} { 
		}
		constexpr Properties(Property r, Property w, Property u) 
				: mask_{(uint32_t)r | (uint32_t)w | (uint32_t)u} {
		}

	public:
		constexpr Properties() 
				: mask_{(uint32_t)Property::NONE} {
		}	

		// Static Constants
		static constexpr Properties none() {
			return Properties{(uint32_t)Property::NONE};
		}

		// Element Operators
		constexpr Properties operator+(Property rhs) {
			return Properties{(uint32_t)rhs};
		}

		constexpr Properties operator-(Property rhs) {
			return Properties{mask_ & ~(uint32_t)rhs};
		}

		Properties& operator+=(Property rhs) {
			mask_ |= ((uint32_t)rhs);
			return *this;
		}

		Properties& operator-=(Property rhs) {
			mask_ &= ~((uint32_t)rhs);
			return *this;
		}

		// Queries
		constexpr bool contains(Property p) {
			return (mask_ & (uint32_t)p) == (uint32_t)p;
		}

	private:
		uint32_t mask_;
};

} // namespace x64asm

#endif
