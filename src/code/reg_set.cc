#include "src/code/reg_set.h"

using namespace std;

namespace x64 {

vector<RH> RegSet::rh_;
vector<R8> RegSet::r8_;
vector<R16> RegSet::r16_;
vector<R32> RegSet::r32_;
vector<R64> RegSet::r64_;
vector<Xmm> RegSet::xmm_;
vector<CondReg> RegSet::cond_regs_;

BitWidth RegSet::get_widest_set(R r) const {
	if ( is_set((R64)r) )
		return QUAD;
	else if ( is_set((R32)r) )
		return DOUBLE;
	else if ( is_set((R16)r) )
		return WORD;
	else if ( is_set((R8)r) )
		return LOW;
	else if ( is_set((RH)r) )
		return HIGH;
	else
		return BIT_WIDTH_NULL;
}

} // namespace x64
