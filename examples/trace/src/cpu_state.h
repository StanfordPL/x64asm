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

#ifndef TRACE_CPU_STATE_H
#define TRACE_CPU_STATE_H

#include <cassert>
#include <stdint.h>
#include <vector>

#include "include/x64asm.h"

#include "def.h"

namespace trace {

/** I/O formatting options for CpuStates. */
enum class Format : uint32_t {
	BIN,
	HEX,
};

/** A datastructure representation of a hardware CPU state. */
class CpuState {
	// Needs access to internal buffers.
	friend x64asm::Function assemble_write_cpu(const CpuState& cs);
	// Needs access to internal buffers.
	friend x64asm::Function assemble_read_cpu(const CpuState& cs);
	// Needs access to internal buffers.
	friend x64asm::Function assemble_read_cpu(const CpuState& cs, 
		const std::vector<x64asm::R64>& rcalls, const std::vector<uint64_t*>& addrs);

	public:
		/** Returns a CpuState which will track all hardware registers. */
		CpuState() 
				: mask_(x64asm::RegSet::universe()) {
			init_fxns();
		}

		/** Returns a CpuState which will track user-specified registers. */
		CpuState(const x64asm::RegSet mask) 
				: mask_(mask) {
			init_fxns();
		}

		/** Returns a CpuState with identical tracking behavior to input. */
		CpuState(const CpuState& rhs);

		/** Returns a CpuState with identical tracking behavior to input. */
		CpuState& operator=(const CpuState& rhs);

		/** Returns a fixed point byte from a general purpose register. */
		uint8_t get_fixed_byte(const x64asm::R64& r, size_t i = 0) const {
			assert(contains(r));
			assert(i < 8);
			return ((uint8_t*)(&gp_[r]))[i];
		}

		/** Returns a fixed point word from a general purpose register. */
		uint16_t get_fixed_word(const x64asm::R64& r, size_t i = 0) const {
			assert(contains(r));
			assert(i < 4);
			return ((uint16_t*)(&gp_[r]))[i];
		}

		/** Returns a fixed point double from a general purpose register. */
		uint32_t get_fixed_double(const x64asm::R64& r, size_t i = 0) const {
			assert(contains(r));
			assert(i < 2);
			return ((uint32_t*)(&gp_[r]))[i];
		}

		/** Returns a fixed point quad from a general purpose register. */
		uint64_t get_fixed_quad(const x64asm::R64& r, size_t i = 0) const {
			assert(contains(r));
			assert(i == 0);
			return gp_[r];
		}

		/** Returns a single precision floating point from a general purpose 
			  register. 
	  */
		float get_float_single(const x64asm::R64& r, size_t i = 0) const {
			assert(contains(r));
			assert(i < 2);
			return ((float*)(&gp_[r]))[i];
		}

		/** Returns a double precision floating point from a general purpose 
			  register. 
	  */
		double get_float_double(const x64asm::R64& r, size_t i = 0) const {
			assert(contains(r));
			assert(i == 0);
			return ((double*)(&gp_[r]))[i];
		}

		/** Returns a fixed point byte from an SSE register. */
		uint8_t get_fixed_byte(const x64asm::SSE_TYPE& s, size_t i = 0) const {
			assert(contains(s));
			assert(i < BYTES_PER_SSE);
			return ((uint8_t*)&sse_[s*SSE_SIZE])[i];
		}

		/** Returns a fixed point word from an SSE register. */
		uint16_t get_fixed_word(const x64asm::SSE_TYPE& s, size_t i = 0) const {
			assert(contains(s));
			assert(i < WORDS_PER_SSE);
			return ((uint16_t*)&sse_[s*SSE_SIZE])[i];
		}

		/** Returns a fixed point double from an SSE register. */
		uint32_t get_fixed_double(const x64asm::SSE_TYPE& s, size_t i = 0) const {
			assert(contains(s));
			assert(i < DOUBLES_PER_SSE);
			return ((uint32_t*)&sse_[s*SSE_SIZE])[i];
		}

		/** Returns a fixed point quad from an SSE register. */
		uint64_t get_fixed_quad(const x64asm::SSE_TYPE& s, size_t i = 0) const {
			assert(contains(s));
			assert(i < QUADS_PER_SSE);
			return ((uint64_t*)&sse_[s*SSE_SIZE])[i];
		}

		/** Returns a single precision floating point from an SSE register. */
		float get_float_single(const x64asm::SSE_TYPE& s, size_t i = 0) const {
			assert(contains(s));
			assert(i < DOUBLES_PER_SSE);
			return ((float*)&sse_[s*SSE_SIZE])[i];
		}

		/** Returns a double precision floating point from an SSE register. */
		double get_float_double(const x64asm::SSE_TYPE& s, size_t i = 0) const {
			assert(contains(s));
			assert(i < QUADS_PER_SSE);
			return ((double*)&sse_[s*SSE_SIZE])[i];
		}

		/** Sets a fixed point byte in a general purpose register. */
		void set_fixed_byte(uint8_t val, const x64asm::R64& r, size_t i = 0) {
			assert(contains(r));
			assert(i < 8);
			((uint8_t*)(&gp_[r]))[i] = val;
		}

		/** Sets a fixed point word in a general purpose register. */
		void set_fixed_word(uint16_t val, const x64asm::R64& r, size_t i = 0) {
			assert(contains(r));
			assert(i < 4);
			((uint16_t*)(&gp_[r]))[i] = val;
		}

		/** Sets a fixed point double in a general purpose register. */
		void set_fixed_double(uint32_t val, const x64asm::R64& r, size_t i = 0) {
			assert(contains(r));
			assert(i < 2);
			((uint32_t*)(&gp_[r]))[i] = val;
		}

		/** Sets a fixed point quad in a general purpose register. */
		void set_fixed_quad(uint64_t val, const x64asm::R64& r, size_t i = 0) {
			assert(contains(r));
			assert(i == 0);
			gp_[r] = val;
		}

		/** Sets a single precision floating point in a general purpose register. */
		void set_float_single(float val, const x64asm::R64& r, size_t i = 0) {
			assert(contains(r));
			assert(i < 2);
			((float*)(&gp_[r]))[i] = val;
		}

		/** Sets a double precision floating point in a general purpose register. */
		void set_float_double(double val, const x64asm::R64& r, size_t i = 0) {
			assert(contains(r));
			assert(i == 0);
			((double*)(&gp_[r]))[i] = val;
		}

		/** Sets a fixed point byte in an SSE register. */
		void set_fixed_byte(uint8_t val, const x64asm::SSE_TYPE& s, size_t i = 0) {
			assert(contains(s));
			assert(i < BYTES_PER_SSE);
			((uint8_t*)(&sse_[s*SSE_SIZE]))[i] = val;
		}

		/** Sets a fixed point word in an SSE register. */
		void set_fixed_word(uint16_t val, const x64asm::SSE_TYPE& s, size_t i = 0) {
			assert(contains(s));
			assert(i < WORDS_PER_SSE);
			((uint16_t*)(&sse_[s*SSE_SIZE]))[i] = val;
		}

		/** Sets a fixed point double in an SSE register. */
		void set_fixed_double(uint32_t val, const x64asm::SSE_TYPE& s, size_t i = 0) {
			assert(contains(s));
			assert(i < DOUBLES_PER_SSE);
			((uint32_t*)(&sse_[s*SSE_SIZE]))[i] = val;
		}

		/** Sets a fixed point quad in an SSE register. */
		void set_fixed_quad(uint64_t val, const x64asm::SSE_TYPE& s, size_t i = 0) {
			assert(contains(s));
			assert(i < QUADS_PER_SSE);
			((uint64_t*)(&sse_[s*SSE_SIZE]))[i] = val;
		}

		/** Sets a single precision floating point in an SSE register. */
		void set_float_single(float val, const x64asm::SSE_TYPE& s, size_t i = 0) {
			assert(contains(s));
			assert(i < DOUBLES_PER_SSE);
			((float*)(&sse_[s*SSE_SIZE]))[i] = val;
		}

		/** Sets a double precision floating point in an SSE register. */
		void set_float_double(double val, const x64asm::SSE_TYPE& s, size_t i = 0) {
			assert(contains(s));
			assert(i < QUADS_PER_SSE);
			((double*)(&sse_[s*SSE_SIZE]))[i] = val;
		}

		/** Returns true for general purpose registers this CpuState is tracking. */
		bool contains(const x64asm::R64& r) const {
			return mask_.contains(r);
		}

		/** Returns true for SSE registers this CpuState is tracking. */
		bool contains(const x64asm::SSE_TYPE& s) const {
			return mask_.contains(s);
		}

		/** Returns true for CpuStates that track identical registers with 
			  identical values. 
		*/
		bool operator==(const CpuState& rhs) const;

		/** Returns true for CpuStates that track different registers or
			  hold different values. 
		*/
		bool operator!=(const CpuState& rhs) const {
			return !(*this == rhs);
		}

		/** Performs bitwise intersection of tracked registers with input. */
		CpuState& operator&=(const CpuState& rhs);

		/** Performs bitwise disjunction of tracked registers with input. */
		CpuState& operator|=(const CpuState& rhs);

		/** Performs bitwise xor of tracked registers with input. */
		CpuState& operator^=(const CpuState& rhs);

		/** Bitwise inversion of tracked registers. */
		CpuState operator~() const;

		/** Bitwise intersection of tracked registers for two CpuStates. */
		CpuState operator&(const CpuState& rhs) const {
			CpuState temp = *this;
			return temp &= rhs;
		}

		/** Bitwise disjunction of tracked registers for two CpuStates. */
		CpuState operator|(const CpuState& rhs) const {
			CpuState temp = *this;
			return temp |= rhs;
		}

		/** Bitwise xor of tracked registers for two CpuStates. */
		CpuState operator^(const CpuState& rhs) const {
			CpuState temp = *this;
			return temp ^= rhs;
		}

		/** Copies the state of tracked registers to hardware. Caution: It is VERY
			  easy to get yourself in trouble with this method.
		*/
		void write_cpu() const {
			write_cpu_.call<void>();
		}

		/** Copies machine state for tracked registers from hardware. Caution: This
			  method relies on your compiler being able to jump directly to this
				function without modifying register state. Some perturbations may be
				observed.
		*/
		void read_cpu() {
			read_cpu_.call<void>();
		}

	private:
		/** General purpose register buffer. */
		std::array<uint64_t, 16> gp_;
		/** SSE register buffer. */
		std::array<uint64_t, SSE_SIZE*16> sse_;
		/** The set of registers being tracked. */
		x64asm::RegSet mask_;

		/** A function for writing the cpu state. */
		x64asm::Function write_cpu_;
		/** A function for reading the cpu state. */
		x64asm::Function read_cpu_;

		/** Reassembles read/write functions for this CpuState. */
		void init_fxns() {
			write_cpu_ = assemble_write_cpu(*this);
			read_cpu_ = assemble_read_cpu(*this);
		}
};

/** Returns a function which writes the input's tracked registers to 
	  hardware. 
*/
x64asm::Function assemble_write_cpu(const CpuState& cs);
/** Returns a function which reads the input's tracked registers from 
	  hardware. @todo This isn't correctly copying %rbp and %rsp.
*/
x64asm::Function assemble_read_cpu(const CpuState& cs);
/** Returns a function which reads the input's tracked registers from
    hardware AFTER restoring the values of rcalls from addrs. Use this when 
		you're forced to clobber the value of some registers prior to calling this 
		function.	@todo This isn't correctly copying %rbp and %rsp. 
*/
x64asm::Function assemble_read_cpu(const CpuState& cs, 
		const std::vector<x64asm::R64>& rcalls, const std::vector<uint64_t*>& addrs);

} // namespace trace

std::istream& operator>>(std::istream& is, trace::CpuState& cs);
std::istream& operator>>(std::istream& is, trace::Format f);

std::ostream& operator<<(std::ostream& os, const trace::CpuState& cs);
std::ostream& operator<<(std::ostream& os, trace::Format f);

#include "undef.h"

#endif
