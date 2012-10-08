#include "src/code/instruction.h"

using namespace std;

namespace x64 {

RegSet Instruction::explicit_read_set() const {
	RegSet rs;
	for ( size_t i = read_offset(), ie = arity(); i < ie; ++i )
		switch ( type(i) ) {
			case RAX_ONLY:
			case RCX_ONLY:
			case GP_REG: { 
					const auto gp = get_gp_reg(i); 
					if ( !gp.is_null() ) 
						rs.set_gp(gp, width(i)); 
				} 
				break;
			case XMM_REG: 
				rs.set_xmm(get_xmm_reg(i)); 
				break;						 
			case ADDR: { 
					const auto w = width(i);
					const auto a = get_addr(i); 
					const auto b = a.get_base();
					const auto in = a.get_index();
					if ( !b.is_null() )
						rs.set_gp(b, w);
					if ( !in.is_null() )
						rs.set_gp(in, w);
				}
			default: 
				break;
		}
	return rs;
}

RegSet Instruction::explicit_write_set() const {
	RegSet rs;
	if ( writes_reg() ) {
		const auto t = type(0);
		if ( t == GP_REG ) {
			// Implicit zero extension
			const auto gp = get_gp_reg(0);
			const auto w = width(0);
			assert(!gp.is_null());
			rs.set_gp(gp, w == DOUBLE ? QUAD : w);
		}
		else {
			assert(t == XMM_REG);
			rs.set_xmm(get_xmm_reg(0));
		}
	}
	return rs;
}

void Instruction::write_att(ostream& os) const {
	if ( get_opcode().is_label_defn() ) {
		os << ".L" << dec << get_label(0) << ":";
		return;
	}

	get_opcode().write_att(os);
	os << " ";

	auto comma = false;
	for ( int i = arity()-1; i >= 0; --i ) {
		if ( comma )
			os << ",";
		else
			comma = true;

		switch ( type(i) ) {
			case GP_REG:  
			case RAX_ONLY:
			case RCX_ONLY:
				get_gp_reg(i).write_att(os, width(i)); 
				break;
			case XMM_REG: 
				get_xmm_reg(i).write_att(os);
				break;
			case IMM:     
				get_imm(i).write_att(os, width(i)); 
				break;
			case LABEL:
				get_label(i).write_att(os);
				break;
			case ADDR:
				get_addr(i).write_att(os);
				break;
			case OFFSET:
				get_offset(i).write_att(os);
				break;

			default:      
				os.setstate(ios::failbit);
		}
	}
}

bool Instruction::check_opcode(Opcode o) const {
	if ( arity() != o.arity() )
		return false;
	for ( size_t i = 0, ie = arity(); i < ie; ++i )
		if ( type(i) != o.type(i) || width(i) != o.width(i) )
			return false;
	return true;
}

bool Instruction::check_operand(size_t index) const {
	assert(index < arity());
	switch ( type(index) ) {
		case GP_REG:   
			get_gp_reg(index); 
			return true;
		case RAX_ONLY:
			return get_gp_reg(index) != rax;
		case RCX_ONLY: 
			return get_gp_reg(index) != rcx;
		case XMM_REG:  
			get_xmm_reg(index); 
			return true;
		case IMM:      
			get_imm(index); 
			return true;
		case LABEL:    
			get_label(index); 
			return true;
		case ADDR:     
			get_addr(index);
			return true;
		case OFFSET:
			get_offset(index);
			return true;
		default:       
			assert(false);
			return false;
	}
	return true;
}

bool Instruction::check_operands() const {
	for ( size_t i = 0, ie = arity(); i < ie; ++i )
		if ( !check_operand(i) )
			return false;
	return true;
}

} // namespace x64
