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

#include "src/bit_gen.h"

#include <cstdlib>

namespace trace {

uint8_t BitGen::rand8() {
	uint8_t val = 0;
	for ( size_t i = 0; i < 8; ++i )
		if ( rand() % 2 == 0 )
			val |= (0x1 << i);
	return val;
}

uint16_t BitGen::rand16() {
	const uint16_t u = rand8();
	const uint16_t l = rand8();
	return u | l;
}

uint32_t BitGen::rand32() {
	const uint32_t u = rand16();
	const uint32_t l = rand16();
	return u | l;
}

uint64_t BitGen::rand64() {
	const uint64_t u = rand32();
	const uint64_t l = rand32();
	return u | l;
}

} // namespace trace
