#include "src/code/instruction.h"

using namespace std;

namespace {

vector<const char*> att_ {{
	// Internal mnemonics
	"<label definition>"
	// Auto-generated mnemonics
	#include "src/code/opcode.att"
}};	

} // namespace

namespace x64 {

OpSet Instruction::explicit_must_read_set() const {
	auto ret = OpSet::empty();
	for ( size_t i = 0, ie = arity(); i < ie; ++i ) {
		const auto o = get_operand(i);
		if ( const auto m = dynamic_cast<const M*>(o) )
			ret += *m;
		else if ( properties(i).contains(Property::MUST_READ) )
			o->insert_in(ret, false);
	}

	return ret;
}

OpSet Instruction::explicit_maybe_read_set() const {
	auto ret = OpSet::empty();
	for ( size_t i = 0, ie = arity(); i < ie; ++i ) {
		const auto o = get_operand(i);
		if ( const auto m = dynamic_cast<const M*>(o) )
			ret += *m;
		else if ( properties(i).contains(Property::MAYBE_READ) )
			o->insert_in(ret, false);
	}

	return ret;
}

OpSet Instruction::explicit_must_write_set() const {
	auto ret = OpSet::empty();
	for ( size_t i = 0, ie = arity(); i < ie; ++i ) {
		const auto o = get_operand(i);
		const auto p = properties(i);
		if ( p.contains(Property::MUST_WRITE_ZX) )
			o->insert_in(ret, true);
		else if ( p.contains(Property::MUST_WRITE) )
			o->insert_in(ret, false);
	}

	return ret;
}

OpSet Instruction::explicit_maybe_write_set() const {
	auto ret = OpSet::empty();
	for ( size_t i = 0, ie = arity(); i < ie; ++i ) {
		const auto o = get_operand(i);
		const auto p = properties(i);
		if ( p.contains(Property::MAYBE_WRITE_ZX) )
			o->insert_in(ret, true);
		else if ( p.contains(Property::MAYBE_WRITE) )
			o->insert_in(ret, false);
	}

	return ret;
}

OpSet Instruction::explicit_must_undef_set() const {
	auto ret = OpSet::empty();
	for ( size_t i = 0, ie = arity(); i < ie; ++i ) 
		if ( properties(i).contains(Property::MUST_UNDEF) )
			get_operand(i)->insert_in(ret, false);

	return ret;
}

OpSet Instruction::explicit_maybe_undef_set() const {
	auto ret = OpSet::empty();
	for ( size_t i = 0, ie = arity(); i < ie; ++i ) 
		if ( properties(i).contains(Property::MAYBE_UNDEF) )
			get_operand(i)->insert_in(ret, false);

	return ret;
}

bool Instruction::check() const {
	for ( size_t i = 0, ie = arity(); i < ie; ++i ) {
		const auto o = get_operand(i);
		if ( type(i) != o->type() || !o->check() )
			return false;
	}
	return true;
}

void Instruction::write_att(ostream& os) const {
	assert(get_opcode() < att_.size());
	os << att_[get_opcode()] << " ";

	if ( arity() > 0 )
		for ( size_t i = arity()-1; i >= 0; ++i ) {
			get_operand(i)->write_att(os);
			if ( i != 0 )
				os << ", ";
		}
}

void Instruction::write_intel(ostream& os) const {
}

vector<size_t> Instruction::arity_ {
	// Internal mnemonics
	0
	// Auto-generated mnemonics
	#include "src/code/arity.table"
};

vector<vector<Properties>> Instruction::properties_ {
	// Internal mnemonics
	{}
	// Auto-generated mnemonics
	#include "src/code/properties.table"
};

vector<vector<OpType>> Instruction::type_ {
	// Internal mnemonics
	{}
	// Auto-generated mnemonics
	#include "src/code/type.table"
};

vector<bool> Instruction::is_return_ {
	// Internal mnemonics
	false
	// Auto-generated mnemonics
	#include "src/code/return.table"
};

vector<bool> Instruction::is_nop_ {
	// Internal mnemonics
	false
	// Auto-generated mnemonics
	#include "src/code/nop.table"
};

vector<bool> Instruction::is_jump_ {
	// Internal mnemonics
	false
	// Auto-generated mnemonics
	#include "src/code/jump.table"
};

vector<bool> Instruction::is_cond_jump_ {
	// Internal mnemonics
	false
	// Auto-generated mnemonics
	#include "src/code/cond_jump.table"
};

vector<bool> Instruction::is_uncond_jump_ {
	// Internal mnemonics
	false
	// Auto-generated mnemonics
	#include "src/code/uncond_jump.table"
};

vector<OpSet> Instruction::implicit_must_read_set_ {
	// Internal mnemonics
	OpSet::empty()
	// Auto-generated mnemonics
	#include "src/code/must_read.table"
};

vector<OpSet> Instruction::implicit_maybe_read_set_ {
	// Internal mnemonics
	OpSet::empty()
	// Auto-generated mnemonics
	#include "src/code/maybe_read.table"
};

vector<OpSet> Instruction::implicit_must_write_set_ {
	// Internal mnemonics
	OpSet::empty()
	// Auto-generated mnemonics
	#include "src/code/must_write.table"
};

vector<OpSet> Instruction::implicit_maybe_write_set_ {
	// Internal mnemonics
	OpSet::empty()
	// Auto-generated mnemonics
	#include "src/code/maybe_write.table"
};

vector<OpSet> Instruction::implicit_must_undef_set_ {
	// Internal mnemonics
	OpSet::empty()
	// Auto-generated mnemonics
	#include "src/code/must_undef.table"
};

vector<OpSet> Instruction::implicit_maybe_undef_set_ {
	// Internal mnemonics
	OpSet::empty()
	// Auto-generated mnemonics
	#include "src/code/maybe_undef.table"
};

} // namespace x64
