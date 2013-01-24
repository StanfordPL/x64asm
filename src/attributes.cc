#include "src/code/attributes.h"

using namespace x64;
using namespace std;

namespace {

template <Property P>
inline OpSet read_set(const Instruction& instr) {
	auto rs = OpSet::empty();
	for ( size_t i = 0, ie = Attributes::arity(instr); i < ie; ++i ) {
		const auto p = Attributes::properties(instr, i);
		const auto o = instr.get_operand(i);

		switch ( Attributes::type(instr, i) ) {
			case OpType::M_8:
			case OpType::M_16:
			case OpType::M_32:
			case OpType::M_64:
			case OpType::M_128:
			case OpType::M_256:
			case OpType::M_PAIR_16_64:
			case OpType::M_PTR_16_16:
			case OpType::M_PTR_16_32:
			case OpType::M_PTR_16_64:
			case OpType::M_16_INT:
			case OpType::M_32_INT:
			case OpType::M_64_INT:
			case OpType::M_32_FP:
			case OpType::M_64_FP:
			case OpType::M_80_FP:
			case OpType::M_2_BYTE:
			case OpType::M_14_BYTE:
			case OpType::M_28_BYTE:
			case OpType::M_94_BYTE:
			case OpType::M_108_BYTE:
			case OpType::M_512_BYTE: rs += (M)o; break;
			case OpType::RH:         if ( p.contains(P) ) rs += (Rh)o; break;
			case OpType::RL:         
			case OpType::AL:         
			case OpType::CL:         if ( p.contains(P) ) rs += (Rl)o; break;
			case OpType::RB:         if ( p.contains(P) ) rs += (Rb)o; break;
			case OpType::R_16:      
			case OpType::AX:         
			case OpType::DX:         if ( p.contains(P) ) rs += (R16)o; break;
			case OpType::R_32:       
			case OpType::EAX:        if ( p.contains(P) ) rs += (R32)o; break;
			case OpType::R_64:       
			case OpType::RAX:        if ( p.contains(P) ) rs += (R64)o; break;
			case OpType::XMM:        
			case OpType::XMM_0:      if ( p.contains(P) ) rs += (Xmm)o; break;
			case OpType::YMM:        if ( p.contains(P) ) rs += (Ymm)o; break;
			default: assert(false);
		}
	}
	return rs;
}

} // namespace

namespace x64 {

} // namespace x64
