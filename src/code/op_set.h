#ifndef X64_SRC_CODE_OP_SET_H
#define X64_SRC_CODE_OP_SET_H

#include "src/code/checker.h"
#include "src/code/eflag.h"
#include "src/code/m.h"
#include "src/code/mm.h"
#include "src/code/r.h"
#include "src/code/xmm.h"
#include "src/code/ymm.h"

namespace x64 {

class OpSet {
	private:
		enum class Mask : uint64_t {
			EMPTY  = 0x0000000000000000,
			UNIV   = 0xffffffffffffffff,
			
			LOW    = 0x0000000000000001,
			HIGH   = 0x0000000000010000,
			WORD   = 0x0000000000010001,
			DOUBLE = 0x0000000100010001,
		 	QUAD   = 0x0001000100010001,

			XMM    = 0x0000000000000001,
			YMM    = 0x0000000000010001,

			MM     = 0x0000000100000000
		};

		inline OpSet(uint64_t g, uint64_t o, uint64_t f)
				: gp_regs_{g}, other_regs_{o}, flags_{f} {
		}

		inline OpSet(Mask g, Mask o, Mask f)
				: gp_regs_{(uint64_t)g}, other_regs_{(uint64_t)o}, flags_{(uint64_t)f} {
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
			return OpSet(~gp_regs_, ~other_regs_, ~flags_);
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
			gp_regs_ &= rhs.gp_regs_;
			other_regs_ &= rhs.other_regs_;
			flags_ &= rhs.flags_;
			return *this;
		}

		inline OpSet& operator|=(const OpSet& rhs) {
			gp_regs_ |= rhs.gp_regs_;
			other_regs_ |= rhs.other_regs_;
			flags_ |= rhs.flags_;
			return *this;
		}

		inline OpSet& operator-=(const OpSet& rhs) {
			gp_regs_ &= ~rhs.gp_regs_;
			other_regs_ &= ~rhs.other_regs_;
			flags_ &= ~rhs.flags_;
			return *this;
		}

		// Comparison Operators
		inline bool operator==(const OpSet& rhs) const {
			return gp_regs_ == rhs.gp_regs_ && other_regs_ == rhs.other_regs_ &&
				     flags_ == rhs.flags_;
		}

		inline bool operator!=(const OpSet& rhs) const {
			return !(*this == rhs);
		}

		// Element Operators
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

		inline OpSet operator+(Xmm rhs) const {
			auto ret = *this;
			return ret += rhs;
		}

		inline OpSet operator+(Ymm rhs) const {
			auto ret = *this;
			return ret += rhs;
		}

		inline OpSet operator+(Mm rhs) const {
			auto ret = *this;
			return ret += rhs;
		}

		inline OpSet operator+(M rhs) const {
			auto ret = *this;
			return ret += rhs;
		}

		// TODO... and so forth

		inline OpSet& operator+=(Rh rhs) {
			assert(Checker::check(rhs));	
			gp_regs_ |= ((uint64_t) Mask::HIGH << (rhs.val_-4));
			return *this;
		}

		inline OpSet& operator+=(Rl rhs) {
			assert(Checker::check(rhs));	
			gp_regs_ |= ((uint64_t) Mask::LOW << rhs.val_);
			return *this;
		}

		inline OpSet& operator+=(Rb rhs) {
			assert(Checker::check(rhs));	
			gp_regs_ |= ((uint64_t) Mask::LOW << rhs.val_);
			return *this;
		}

		inline OpSet& operator+=(R16 rhs) {
			assert(Checker::check(rhs));	
			gp_regs_ |= ((uint64_t) Mask::WORD << rhs.val_);
			return *this;
		}

		inline OpSet& operator+=(R32 rhs) {
			assert(Checker::check(rhs));	
			gp_regs_ |= ((uint64_t) Mask::DOUBLE << rhs.val_);
			return *this;
		}

		inline OpSet& operator+=(R64 rhs) {
			assert(Checker::check(rhs));	
			gp_regs_ |= ((uint64_t) Mask::QUAD << rhs.val_);
			return *this;
		}

		inline OpSet& operator+=(Xmm rhs) {
			assert(Checker::check(rhs));	
			other_regs_ |= ((uint64_t) Mask::XMM << rhs.val_);
			return *this;
		}

		inline OpSet& operator+=(Ymm rhs) {
			assert(Checker::check(rhs));	
			other_regs_ |= ((uint64_t) Mask::YMM << rhs.val_);
			return *this;
		}

		inline OpSet& operator+=(Mm rhs) {
			assert(Checker::check(rhs));	
			other_regs_ |= ((uint64_t) Mask::MM << rhs.val_);
			return *this;
		}

		OpSet& operator+=(M rhs);

		// TODO... and so forth	

		// Queries

		inline bool contains(Rh rhs) const {
			assert(Checker::check(rhs));
			return ((gp_regs_ >> (rhs.val_-4)) & (uint64_t)Mask::HIGH) == 
				     (uint64_t)Mask::HIGH;
		}

		inline bool contains(Rl rhs) const {
			assert(Checker::check(rhs));
			return ((gp_regs_ >> rhs.val_) & (uint64_t)Mask::LOW) == 
				     (uint64_t)Mask::LOW;
		}

		inline bool contains(Rb rhs) const {
			assert(Checker::check(rhs));
			return ((gp_regs_ >> rhs.val_) & (uint64_t)Mask::LOW) == 
				     (uint64_t)Mask::LOW;
		}

		inline bool contains(R16 rhs) const {
			assert(Checker::check(rhs));
			return ((gp_regs_ >> rhs.val_) & (uint64_t)Mask::WORD) == 
				     (uint64_t)Mask::WORD;
		}

		inline bool contains(R32 rhs) const {
			assert(Checker::check(rhs));
			return ((gp_regs_ >> rhs.val_) & (uint64_t)Mask::DOUBLE) == 
				     (uint64_t)Mask::DOUBLE;
		}

		inline bool contains(R64 rhs) const {
			assert(Checker::check(rhs));
			return ((gp_regs_ >> rhs.val_) & (uint64_t)Mask::QUAD) == 
				     (uint64_t)Mask::QUAD;
		}

		inline bool contains(Xmm rhs) const {
			assert(Checker::check(rhs));
			return ((other_regs_ >> rhs.val_) & (uint64_t)Mask::XMM) == 
				     (uint64_t)Mask::XMM;
		}

		inline bool contains(Ymm rhs) const {
			assert(Checker::check(rhs));
			return ((other_regs_ >> rhs.val_) & (uint64_t)Mask::YMM) == 
				     (uint64_t)Mask::YMM;
		}

		inline bool contains(Mm rhs) const {
			assert(Checker::check(rhs));
			return ((other_regs_ >> rhs.val_) & (uint64_t)Mask::MM) == 
				     (uint64_t)Mask::MM;
		}

	private:
		// GP regs [63-0]
		uint64_t gp_regs_;
		// YMM/XMM regs [31-0]
		// MM/FP regs   [39-32]
		uint64_t other_regs_;
		// EFLAGS etc...
		uint64_t flags_;
};

} // namespace x64

#endif
