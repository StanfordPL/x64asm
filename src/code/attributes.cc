#include "src/code/attributes.h"

using namespace std;

namespace x64 {
#if 0
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
#endif

OpSet Attributes::explicit_read_set(const Instruction& i) {
	// TODO
	return OpSet::empty();
}

OpSet Attributes::explicit_write_set(const Instruction& i) {
	// TODO
	return OpSet::empty();
}

OpSet Attributes::explicit_def_set(const Instruction& i) {
	// TODO
	return OpSet::empty();
}

OpSet Attributes::explicit_undef_set(const Instruction& i) {
	// TODO
	return OpSet::empty();
}

vector<size_t> Attributes::arity_ {{
	// Internal mnemonics
	0
	// Auto-generated mnemonics
	#include "src/code/arity.table"
}};

vector<vector<OpAccessor>> Attributes::accessor_ {{
	// Internal mnemonics
	{{}}
	// Auto-generated mnemonics
	#include "src/code/accessor.table"
}};

vector<vector<OpType>> Attributes::type_ {{
	// Internal mnemonics
	{{}}
	// Auto-generated mnemonics
	#include "src/code/type.table"
}};

vector<bool> Attributes::is_return_ {{
	// Internal mnemonics
	false
	// Auto-generated mnemonics
	#include "src/code/return.table"
}};

vector<bool> Attributes::is_nop_ {{
	// Internal mnemonics
	false
	// Auto-generated mnemonics
	#include "src/code/nop.table"
}};

vector<bool> Attributes::is_jump_ {{
	// Internal mnemonics
	false
	// Auto-generated mnemonics
	#include "src/code/jump.table"
}};

vector<bool> Attributes::is_cond_jump_ {{
	// Internal mnemonics
	false
	// Auto-generated mnemonics
	#include "src/code/cond_jump.table"
}};

vector<bool> Attributes::is_uncond_jump_ {{
	// Internal mnemonics
	false
	// Auto-generated mnemonics
	#include "src/code/uncond_jump.table"
}};

vector<OpSet> Attributes::implicit_read_set_ {{
	// Internal mnemonics
	OpSet::empty()
	// Auto-generated mnemonics
	#include "src/code/read.table"
}};

vector<OpSet> Attributes::implicit_write_set_ {{
	// Internal mnemonics
	OpSet::empty()
	// Auto-generated mnemonics
	#include "src/code/write.table"
}};

vector<OpSet> Attributes::implicit_def_set_ {{
	// Internal mnemonics
	OpSet::empty()
	// Auto-generated mnemonics
	#include "src/code/def.table"
}};

vector<OpSet> Attributes::implicit_undef_set_ {{
	// Internal mnemonics
	OpSet::empty()
	// Auto-generated mnemonics
	#include "src/code/undef.table"
}};

} // namespace x64
