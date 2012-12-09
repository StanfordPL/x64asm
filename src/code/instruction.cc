#include "src/code/instruction.h"

#include "src/code/mm.h"
#include "src/code/moffs.h"
#include "src/code/st.h"
#include "src/code/xmm.h"
#include "src/code/ymm.h"

using namespace std;
using namespace x64;

namespace {

inline void set_mem(RegSet& rs, M m) {
	const auto b = m.get_base();
	const auto i = m.get_index();
	
	if ( m.get_size_or() ) {
		if ( !b.is_null() )
			rs.set((R32)b);
		if ( !i.is_null() )
			rs.set((R32)i);
	}
	else {
		if ( !b.is_null() )
			rs.set((R64)b);
		if ( !i.is_null() )
			rs.set((R64)i);
	}
}

} // namespace

namespace x64 {

bool Instruction::is_null() const {
	if ( get_opcode().is_null() )
		return true;

	for ( size_t i = 0, ie = arity(); i < ie; ++i )
		switch ( type(i) ) {
			case R_H: if ( ((RH)operands_[i]).is_null() ) return true; break;
			case AL: if ( ((Al)operands_[i]).is_null() ) return true; break;
			case CL: if ( ((Cl)operands_[i]).is_null() ) return true; break;
			case AX: if ( ((Ax)operands_[i]).is_null() ) return true; break;
			case EAX: if ( ((Eax)operands_[i]).is_null() ) return true; break;
			case RAX: if ( ((Rax)operands_[i]).is_null() ) return true; break;
			case R_8: 
			case R_16: 
			case R_32: 
			case R_64: if ( ((R)operands_[i]).is_null() ) return true; break;
			case IMM_8: 
			case IMM_16: 
			case IMM_32: 
			case IMM_64: if ( ((Imm)operands_[i]).is_null() ) return true; break;
			case M_8:    
			case M_16:    
			case M_32:    
			case M_64:    
			case M_80:    
			case M_128:    
			case M_256: if ( ((M)operands_[i]).is_null() ) return true; break;
			case MOFFS_8:		 
			case MOFFS_16:		 
			case MOFFS_32:		 
			case MOFFS_64: if ( ((Moffs)operands_[i]).is_null() ) return true; break;
			case SREG: if ( ((Sreg)operands_[i]).is_null() ) return true; break;
			case ST0: if ( ((St0)operands_[i]).is_null() ) return true; break;
			case ST: if ( ((St)operands_[i]).is_null() ) return true; break;
			case MM: if ( ((Mm)operands_[i]).is_null() ) return true; break;
			case XMM: if ( ((Xmm)operands_[i]).is_null() ) return true; break;
			case YMM: if ( ((Ymm)operands_[i]).is_null() ) return true; break;

			default:
				assert(false);
				return true;
		}
	return false;
}

RegSet Instruction::explicit_read_set() const {
	RegSet rs;
	for ( size_t i = opcode_.first_read(), ie = arity(); i < ie; ++i )
		switch ( type(i) ) {
			case R_H: rs.set((RH)operands_[i]); break;
			case AL:
			case CL:
			case R_8: rs.set((R8)operands_[i]); break;
			case AX:
			case R_16: rs.set((R16)operands_[i]); break;
			case EAX:
			case R_32: rs.set((R32)operands_[i]); break;
			case RAX:
			case R_64: rs.set((R64)operands_[i]); break;
			case M_8:    
			case M_16:    
			case M_32:    
			case M_64:    
			case M_80:    
			case M_128:    
			case M_256: set_mem(rs, (M)operands_[i]); break;
			case ST0:
			case ST: break; // TODO 
			case MM: break; // TODO
			case XMM: rs.set((Xmm)operands_[i]); break;
			case YMM: break; // TODO

			default: 
				break;
		}
	return rs;
}

RegSet Instruction::explicit_write_set() const {
	RegSet rs;
	for ( size_t i = 0, ie = opcode_.num_writes(); i < ie; ++i )
		switch ( type(i) ) {
			case R_H: rs.set((RH)operands_[i]); break;
			case AL:
			case CL:
			case R_8: rs.set((R8)operands_[i]); break;
			case AX:
			case R_16: rs.set((R16)operands_[i]); break;
			case EAX:
			case R_32: rs.set((R64)operands_[i]); break; // Implicit extend!!!
			case RAX:
			case R_64: rs.set((R64)operands_[i]); break;
			case ST: break; // TODO 
			case MM: break; // TODO
			case XMM: rs.set((Xmm)operands_[i]); break;
			case YMM: break; // TODO

			default: 
				break;
		}
	return rs;
}

} // namespace x64
