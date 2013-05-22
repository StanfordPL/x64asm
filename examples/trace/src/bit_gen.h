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

#ifndef TRACE_BIT_GEN_H
#define TRACE_BIT_GEN_H

#include <stdint.h>

namespace trace {

/** A utility class for generating bit patterns. */
class BitGen {
	public:
		/** Returns 8 bits of zeros in a fixed point byte. */
		static uint8_t zeros_fixed_byte() {
			return 0;
		}

		/** Returns 16 bits of zeros in a fixed point word. */
		static uint16_t zeros_fixed_word() {
			return 0;
		}

		/** Returns 32 bits of zeros in a fixed point double. */
		static uint32_t zeros_fixed_double() {
			return 0;
		}

		/** Returns 64 bits of zeros in a fixed point quad. */
		static uint64_t zeros_fixed_quad() {
			return 0;
		}

		/** Returns 32 bits of zeros in a single precision floating point. */
		static float zeros_float_single() {
			return 0.0;
		}

		/** Returns 64 bits of zeros in a double precision floating point. */
		static double zeros_float_double() {
			return 0.0;
		}

		/** Returns 8 bits of ones in a fixed point byte. */
		static uint8_t ones_fixed_byte() {
			return -1;
		}

		/** Returns 16 bits of ones in a fixed point word. */
		static uint16_t ones_fixed_word() {
			return -1;
		}

		/** Returns 32 bits of ones in a fixed point double. */
		static uint32_t ones_fixed_double() {
			return -1;
		}

		/** Returns 64 bits of ones in a fixed point quad. */
		static uint64_t ones_fixed_quad() {
			return -1;
		}

		/** Returns 32 bits of ones in a single precision floating point. */
		static float ones_float_single() {
			return 0xffffffff;
		}

		/** Returns 64 bits of ones in a double precision floating point. */
		static double ones_float_double() {
			return 0xffffffffffffffff;
		}

		/** Returns 8 random bits in a fixed point byte. */
		static uint8_t rand_fixed_byte() {
			return rand8();
		}

		/** Returns 16 random bits in a fixed point word. */
		static uint16_t rand_fixed_word() {
			return rand16();
		}

		/** Returns 32 random bits in a fixed point double. */
		static uint32_t rand_fixed_double() {
			return rand32();
		}

		/** Returns 64 random bits in a fixed point quad. */
		static uint64_t rand_fixed_quad() {
			return rand64();
		}

		/** Returns 32 random bits in a single precision floating point. */
		static float rand_float_single() {
			const auto temp = rand32();
			return *((float*)&temp);
		}

		/** Returns 64 random bits in a double precision floating point. */
		static double rand_float_double() {
			const auto temp = rand64();
			return *((double*)&temp);
		}

		/** Returns 32 random bits in a single precision floating point. This
		    value is guaranteed to be normal.
		*/
		static float rand_normal_float_single() {
			float f;
			for ( f = rand_float_single(); is_denormal(f); f = rand_float_single() );
			return f;
		}
		
		/** Returns 64 random bits in a double precision floating point. This
		    value is guaranteed to be normal.
		*/
		static float rand_normal_float_double() {
			double d;
			for ( d = rand_float_double(); is_denormal(d); d = rand_float_double() );
			return d;
		}

	private:
		/** Returns a random 8 bit string. @todo Use a better generator. */
		static uint8_t rand8();
		/** Returns a random 16 bit string. @todo Use a better generator. */
		static uint16_t rand16();
		/** Returns a random 32 bit string. @todo Use a better generator. */
		static uint32_t rand32();
		/** Returns a random 64 bit string. @todo Use a better generator. */
		static uint64_t rand64();

		/** Returns true for normal single precision floating point values. */
		static bool is_normal(float f) {
			return !is_denormal(f);
		}

		/** Returns true for denormal single precision floating point values. */
		static bool is_denormal(float f) {
			const auto val = *((uint32_t*)&f);
			const auto exp = (val >> 23) & 0xff;
			return exp == 0x00 || exp == 0xff;
		}

		/** Returns true for normal double precision floating point values. */
		static bool is_normal(double d) {
			return !is_denormal(d);
		}

		/** Returns true for denormal double precision floating point values. */
		static bool is_denormal(double d) {
			const auto val = *((uint64_t*)&d);
			const auto exp = (val >> 52) & 0x7ff;
			return exp == 0x000 || exp == 0x7ff;
		}
};

} // namespace trace

#endif
