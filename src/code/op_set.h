#ifndef X64_SRC_ATTRIBUTES_OP_SET_H
#define X64_SRC_ATTRIBUTES_OP_SET_H

#include "src/code/eflag.h"
#include "src/code/r.h"
#include "src/code/sreg.h"
#include "src/code/xmm.h"
#include "src/code/ymm.h"

namespace x64 {

class OpSet {
	public:


		// TODO
		inline OpSet operator~() const {
			return *this;
		}

		inline OpSet operator&(const OpSet& os) const {
			return *this;
		}

		inline OpSet operator|(const OpSet& os) const {
			return *this;
		}

		inline OpSet operator^(const OpSet& os) const {
			return *this;
		}

		inline OpSet operator-(const OpSet& os) const {
			return *this;
		}

		inline OpSet& operator&=(const OpSet& os) {
			return *this;
		}

		inline OpSet& operator|=(const OpSet& os) {
			return *this;
		}

		inline OpSet& operator^=(const OpSet& os) {
			return *this;
		}

		inline OpSet& operator-=(const OpSet& os) {
			return *this;
		}

		inline bool operator==(const OpSet& os) {
			return true;
		}

		inline bool operator!=(const OpSet& os) {
			return true;
		}

	private:
		int x_;
};

} // namespace x64

#endif
