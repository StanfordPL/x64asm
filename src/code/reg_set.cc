#include "src/code/reg_set.h"

using namespace std;

namespace x64 {

vector<GpReg> RegSet::gp_regs_;
vector<XmmReg> RegSet::xmm_regs_;
vector<CondReg> RegSet::cond_regs_;

BitWidth RegSet::get_widest_set(GpReg r) const {
	if ( is_set(r, QUAD) )
		return QUAD;
	else if ( is_set(r, DOUBLE) )
		return DOUBLE;
	else if ( is_set(r, WORD) )
		return WORD;
	else if ( is_set(r, HIGH) )
		return HIGH;
	else if ( is_set(r, LOW) )
		return LOW;
	else
		return BIT_WIDTH_NULL;
}

} // namespace x64
