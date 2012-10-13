#include "src/code/instruction.h"

using namespace std;

namespace x64 {

bool Instruction::is_null() const {
	if ( get_opcode().is_null() )
		return true;
	for ( size_t i = 0, ie = arity(); i < ie; ++i )
		switch ( type(i) ) {
			case ADDR:     if ( ((M)operands_[i]).is_null() ) return true;
										 break;
			case ST0_ONLY:
			case FP_REG:   if ( ((St)operands_[i]).is_null() ) return true;
										 break;
			case RAX_ONLY:
			case RCX_ONLY:
			case GP_REG:   if ( ((R)operands_[i]).is_null() ) return true;
									   break;
			case IMM:      if ( ((Imm)operands_[i]).is_null() ) return true;
										 break;
			case LABEL:    if ( ((Label)operands_[i]).is_null() ) return true;
										 break;
			case MMX_REG:  if ( ((Mm)operands_[i]).is_null() ) return true;
										 break;
			case XMM_REG:  if ( ((Xmm)operands_[i]).is_null() ) return true;
										 break;
			default:
				assert(false);
		}
	return false;
}

RegSet Instruction::explicit_read_set() const {
	RegSet rs;
	for ( size_t i = opcode_.first_read(), ie = arity(); i < ie; ++i )
		switch ( type(i) ) {
			case RAX_ONLY:
			case RCX_ONLY:
			case GP_REG: { 
					const auto r = (R) get_operand(i); 
					if ( !r.is_null() )
						switch ( width(i) ) {
							case LOW: rs.set((R8)r); break;
							case HIGH: rs.set((RH)r); break;
							case WORD: rs.set((R16)r); break;
							case DOUBLE: rs.set((R32)r); break;
							case QUAD: rs.set((R64)r); break;
							default:
												 assert(false);
						}
				} 
				break;
			case XMM_REG: 
				rs.set((Xmm)get_operand(i)); 
				break;						 
			case ADDR: { 
					const auto a = (M)get_operand(i); 
					const auto w = a.get_reg_width();
					const auto b = a.get_base();
					const auto idx = a.get_index();
					if ( !b.is_null() ) {
						if ( w == QUAD )
							rs.set((R64)b);
						else
							rs.set((R32)b);
					}
					if ( !idx.is_null() ) {
						if ( w == QUAD )
							rs.set((R64)idx);
						else
							rs.set((R32)idx);
					}
				}
			default: 
				break;
		}

	return rs;
}

RegSet Instruction::explicit_write_set() const {
	RegSet rs;
	for ( size_t i = 0, ie = opcode_.num_writes(); i < ie; ++i )
		switch ( type(i) ) {
			case GP_REG:
			case RAX_ONLY:
			case RCX_ONLY: {
				const auto r = (R)get_operand(i);
				assert(!r.is_null());
				switch ( width(i) ) {
					case LOW: rs.set((R8)r); break;
					case HIGH: rs.set((RH)r); break;
					case WORD: rs.set((R16)r); break;
					case DOUBLE: rs.set((R32)r); break;
					case QUAD: rs.set((R64)r); break;
					default:
						assert(false);
				}
			}
			case XMM_REG:
				rs.set((Xmm)get_operand(i));
				break;

			default:
				assert(false);
		}

	return rs;
}

} // namespace x64
