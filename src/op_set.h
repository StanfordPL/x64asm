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

#ifndef X64ASM_SRC_OP_SET_H
#define X64ASM_SRC_OP_SET_H

#include <iostream>

#include "src/control.h"
#include "src/eflags.h"
#include "src/m.h"
#include "src/mm.h"
#include "src/mxcsr.h"
#include "src/r.h"
#include "src/status.h"
#include "src/tag.h"
#include "src/xmm.h"
#include "src/ymm.h"

namespace x64asm {

class OpSet {
	private:
		enum class Mask : uint64_t {
			EMPTY   = 0x0000000000000000,
			UNIV    = 0xffffffffffffffff,
			
			// Group 1
			LOW     = 0x0000000000000001,
			HIGH    = 0x0000000000010000,
			WORD    = 0x0000000000010001,
			DOUBLE  = 0x0000000100010001,
		 	QUAD    = 0x0001000100010001,

			// Group 2
			XMM     = 0x0000000000000001,
			YMM     = 0x0000000000010001,
			MM      = 0x0000000100000000,
			MXCSR   = 0x0000010000000000,

			// Group 3
			EFLAG   = 0x0000000000000001,
			CONTROL = 0x0000000000400000,
			STATUS  = 0x0000000800000000,
			TAG     = 0x0008000000000000
		};

		inline OpSet(uint64_t g1, uint64_t g2, uint64_t g3)
				: group1_{g1}, group2_{g2}, group3_{g3} {
		}

		inline OpSet(Mask g1, Mask g2, Mask g3)
				: group1_{(uint64_t)g1}, group2_{(uint64_t)g2}, group3_{(uint64_t)g3} {
		}

	public:	
		// Static Constants
		static inline OpSet empty() {
			return OpSet(Mask::EMPTY, Mask::EMPTY, Mask::EMPTY);
		}

		static inline OpSet universe() {
			return OpSet(Mask::UNIV, Mask::UNIV, Mask::UNIV);	
		}

		// Set Operators
		inline OpSet operator~() const {
			return OpSet(~group1_, ~group2_, ~group3_);
		}

		inline OpSet operator&(const OpSet& rhs) const {
			auto ret = *this;
			return ret &= rhs;
		}

		inline OpSet operator|(const OpSet& rhs) const {
			auto ret = *this;
			return ret |= rhs;
		}

		inline OpSet operator-(const OpSet& rhs) const {
			auto ret = *this;
			return ret -= rhs;
		}

		inline OpSet& operator&=(const OpSet& rhs) {
			group1_ &= rhs.group1_;
			group2_ &= rhs.group2_;
			group3_ &= rhs.group3_;
			return *this;
		}

		inline OpSet& operator|=(const OpSet& rhs) {
			group1_ |= rhs.group1_;
			group2_ |= rhs.group2_;
			group3_ |= rhs.group3_;
			return *this;
		}

		inline OpSet& operator-=(const OpSet& rhs) {
			group1_ &= ~rhs.group1_;
			group2_ &= ~rhs.group2_;
			group3_ &= ~rhs.group3_;
			return *this;
		}

		// Comparison Operators
		inline bool operator==(const OpSet& rhs) const {
			return group1_ == rhs.group1_ && group2_ == rhs.group2_ &&
				     group3_ == rhs.group3_;
		}

		inline bool operator!=(const OpSet& rhs) const {
			return !(*this == rhs);
		}

		// Element Operators
		inline OpSet operator+(Control rhs) const {
			auto ret = *this;
			return ret += rhs;
		}

		inline OpSet operator+(Eflags rhs) const {
			auto ret = *this;
			return ret += rhs;
		}

		inline OpSet operator+(Rh rhs) const {
			auto ret = *this;
			return ret += rhs;
		}

		inline OpSet operator+(Rl rhs) const {
			auto ret = *this;
			return ret += rhs;
		}

		inline OpSet operator+(Rb rhs) const {
			auto ret = *this;
			return ret += rhs;
		}

		inline OpSet operator+(R16 rhs) const {
			auto ret = *this;
			return ret += rhs;
		}

		inline OpSet operator+(R32 rhs) const {
			auto ret = *this;
			return ret += rhs;
		}

		inline OpSet operator+(R64 rhs) const {
			auto ret = *this;
			return ret += rhs;
		}

		inline OpSet operator+(Mm rhs) const {
			auto ret = *this;
			return ret += rhs;
		}

		inline OpSet operator+(Mxcsr rhs) const {
			auto ret = *this;
			return ret += rhs;
		}

		inline OpSet operator+(Status rhs) const {
			auto ret = *this;
			return ret += rhs;
		}

		inline OpSet operator+(Tag rhs) const {
			auto ret = *this;
			return ret += rhs;
		}

		inline OpSet operator+(Xmm rhs) const {
			auto ret = *this;
			return ret += rhs;
		}

		inline OpSet operator+(Ymm rhs) const {
			auto ret = *this;
			return ret += rhs;
		}

		inline OpSet operator+(const M& rhs) const {
			auto ret = *this;
			return ret += rhs;
		}

		inline OpSet& operator+=(Control rhs) {
			group3_ |= ((uint64_t) Mask::CONTROL << rhs.val());
			return *this;
		}

		inline OpSet& operator+=(Eflags rhs) {
			group3_ |= ((uint64_t) Mask::EFLAG << rhs.val());
			return *this;
		}

		inline OpSet& operator+=(Rh rhs) {
			group1_ |= ((uint64_t) Mask::HIGH << (rhs.val()-4));
			return *this;
		}

		inline OpSet& operator+=(Rl rhs) {
			group1_ |= ((uint64_t) Mask::LOW << rhs.val());
			return *this;
		}

		inline OpSet& operator+=(Rb rhs) {
			group1_ |= ((uint64_t) Mask::LOW << rhs.val());
			return *this;
		}

		inline OpSet& operator+=(R16 rhs) {
			group1_ |= ((uint64_t) Mask::WORD << rhs.val());
			return *this;
		}

		inline OpSet& operator+=(R32 rhs) {
			group1_ |= ((uint64_t) Mask::DOUBLE << rhs.val());
			return *this;
		}

		inline OpSet& operator+=(R64 rhs) {
			group1_ |= ((uint64_t) Mask::QUAD << rhs.val());
			return *this;
		}

		inline OpSet& operator+=(Mm rhs) {
			group2_ |= ((uint64_t) Mask::MM << rhs.val());
			return *this;
		}

		inline OpSet& operator+=(Mxcsr rhs) {
			group2_ |= ((uint64_t) Mask::MXCSR << rhs.val());
			return *this;
		}

		inline OpSet& operator+=(Status rhs) {
			group3_ |= ((uint64_t) Mask::STATUS << rhs.val());
			return *this;
		}

		inline OpSet& operator+=(Tag rhs) {
			group3_ |= ((uint64_t) Mask::TAG << rhs.val());
			return *this;
		}

		inline OpSet& operator+=(Xmm rhs) {
			group2_ |= ((uint64_t) Mask::XMM << rhs.val());
			return *this;
		}

		inline OpSet& operator+=(Ymm rhs) {
			group2_ |= ((uint64_t) Mask::YMM << rhs.val());
			return *this;
		}

		OpSet& operator+=(const M& rhs);

		// Queries

		inline bool contains(Control rhs) const {
			return ((group3_ >> rhs.val()) & (uint64_t)Mask::CONTROL) == 
				     (uint64_t)Mask::CONTROL;
		}

		inline bool contains(Eflags rhs) const {
			return ((group3_ >> rhs.val()) & (uint64_t)Mask::EFLAG) == 
				     (uint64_t)Mask::EFLAG;
		}

		inline bool contains(Rh rhs) const {
			return ((group1_ >> (rhs.val()-4)) & (uint64_t)Mask::HIGH) == 
				     (uint64_t)Mask::HIGH;
		}

		inline bool contains(Rl rhs) const {
			return ((group1_ >> rhs.val()) & (uint64_t)Mask::LOW) == 
				     (uint64_t)Mask::LOW;
		}

		inline bool contains(Rb rhs) const {
			return ((group1_ >> rhs.val()) & (uint64_t)Mask::LOW) == 
				     (uint64_t)Mask::LOW;
		}

		inline bool contains(R16 rhs) const {
			return ((group1_ >> rhs.val()) & (uint64_t)Mask::WORD) == 
				     (uint64_t)Mask::WORD;
		}

		inline bool contains(R32 rhs) const {
			return ((group1_ >> rhs.val()) & (uint64_t)Mask::DOUBLE) == 
				     (uint64_t)Mask::DOUBLE;
		}

		inline bool contains(R64 rhs) const {
			return ((group1_ >> rhs.val()) & (uint64_t)Mask::QUAD) == 
				     (uint64_t)Mask::QUAD;
		}

		inline bool contains(Mm rhs) const {
			return ((group2_ >> rhs.val()) & (uint64_t)Mask::MM) == 
				     (uint64_t)Mask::MM;
		}

		inline bool contains(Mxcsr rhs) const {
			return ((group2_ >> rhs.val()) & (uint64_t)Mask::MXCSR) == 
				     (uint64_t)Mask::MXCSR;
		}

		inline bool contains(Status rhs) const {
			return ((group3_ >> rhs.val()) & (uint64_t)Mask::STATUS) == 
				     (uint64_t)Mask::STATUS;
		}

		inline bool contains(Tag rhs) const {
			return ((group3_ >> rhs.val()) & (uint64_t)Mask::TAG) == 
				     (uint64_t)Mask::TAG;
		}

		inline bool contains(Xmm rhs) const {
			return ((group2_ >> rhs.val()) & (uint64_t)Mask::XMM) == 
				     (uint64_t)Mask::XMM;
		}

		inline bool contains(Ymm rhs) const {
			return ((group2_ >> rhs.val()) & (uint64_t)Mask::YMM) == 
				     (uint64_t)Mask::YMM;
		}

		void write_att(std::ostream& os) const;
		void write_intel(std::ostream& os) const;

	private:
		// GP regs [63-0]
		uint64_t group1_;
		// YMM/XMM regs [31-0]
		// MM/FP regs   [39-32]
		// MXCSR reg    [55-40]
		uint64_t group2_;
		// EFLAGS  [21-0]
		// Control [34-22]   
		// Status  [50-35]
		// Tag     [58-51]
		uint64_t group3_;

		void write_txt(std::ostream& os, bool att) const;
};

} // namespace x64asm

#endif
