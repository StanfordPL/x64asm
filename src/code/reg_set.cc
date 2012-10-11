#include "src/code/reg_set.h"

using namespace std;

namespace x64 {

list<GpReg> RegSet::gp_regs_;
list<XmmReg> RegSet::xmm_regs_;
list<CondReg> RegSet::cond_regs_;

BitWidth RegSet::get_widest_set(GpReg r) const {
	if ( is_set_gp(r, QUAD) )
		return QUAD;
	else if ( is_set_gp(r, DOUBLE) )
		return DOUBLE;
	else if ( is_set_gp(r, WORD) )
		return WORD;
	else if ( is_set_gp(r, HIGH) )
		return HIGH;
	else if ( is_set_gp(r, LOW) )
		return LOW;
	else
		return BIT_WIDTH_NULL;
}

} // namespace x64
