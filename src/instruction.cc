/*
Copyright 2103 eric schkufza

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

#include "src/instruction.h"

using namespace std;

namespace {

vector<const char*> att_ {
	// Internal mnemonics
	"<label definition>"
	// Auto-generated mnemonics
	#include "src/opcode.att"
};	

vector<const char*> intel_ {
	// Internal mnemonics
	"<label definition>"
	// Auto-generated mnemonics
	#include "src/opcode.intel"
};	

} // namespace

namespace x64asm {

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
		if ( !o->check() )
			return false;

		const auto t = o->type();
		switch ( type(i) ) {
			case OpType::IMM_8: 
				if ( t != OpType::IMM_8 && t != OpType::ZERO && t != OpType::ONE &&
						 t != OpType::THREE )
					return false;
				break;
			case OpType::RL:
				if ( t != OpType::RL && t != OpType::AL && t != OpType::CL )
					return false;
				break;
			case OpType::R_16:
				if ( t != OpType::R_16 && t != OpType::AX && t != OpType::DX )
					return false;
				break;
			case OpType::R_32:
				if ( t != OpType::R_32 && t != OpType::EAX )
					return false;
				break;
			case OpType::R_64:
				if ( t != OpType::R_64 && t != OpType::RAX )
					return false;
				break;
			case OpType::SREG:
				if ( t != OpType::SREG && t != OpType::FS && t != OpType::GS )
					return false;
				break;
			case OpType::XMM:
				if ( t != OpType::XMM && t != OpType::XMM_0 )
					return false;
				break;

			default:
				if ( t != type(i) )
					return false;
		}
	}
	return true;
}

void Instruction::write_att(ostream& os) const {
	assert(get_opcode() < att_.size());
	os << att_[get_opcode()] << " ";

	if ( arity() > 0 )
		for ( int i = arity()-1; i >= 0; --i ) {
			get_operand(i)->write_att(os);
			if ( i != 0 )
				os << ", ";
		}
}

void Instruction::write_intel(ostream& os) const {
	assert(get_opcode() < intel_.size());
	os << intel_[get_opcode()] << " ";

	for ( size_t i = 0, ie = arity(); i < ie; ++i ) {
		get_operand(i)->write_intel(os);
		if ( (i+1) != ie )
			os << ", ";
	}
}

vector<size_t> Instruction::arity_ {
	// Internal mnemonics
	0
	// Auto-generated mnemonics
	#include "src/arity.table"
};

vector<vector<Properties>> Instruction::properties_ {
	// Internal mnemonics
	{}
	// Auto-generated mnemonics
//	#include "src/properties.table"
};

vector<vector<OpType>> Instruction::type_ {
	// Internal mnemonics
	{}
	// Auto-generated mnemonics
	#include "src/type.table"
};

vector<bool> Instruction::is_return_ {
	// Internal mnemonics
	false
	// Auto-generated mnemonics
	#include "src/return.table"
};

vector<bool> Instruction::is_nop_ {
	// Internal mnemonics
	false
	// Auto-generated mnemonics
	#include "src/nop.table"
};

vector<bool> Instruction::is_jump_ {
	// Internal mnemonics
	false
	// Auto-generated mnemonics
	#include "src/jump.table"
};

vector<bool> Instruction::is_cond_jump_ {
	// Internal mnemonics
	false
	// Auto-generated mnemonics
	#include "src/cond_jump.table"
};

vector<bool> Instruction::is_uncond_jump_ {
	// Internal mnemonics
	false
	// Auto-generated mnemonics
	#include "src/uncond_jump.table"
};

vector<OpSet> Instruction::implicit_must_read_set_ {
	// Internal mnemonics
	OpSet::empty()
	// Auto-generated mnemonics
	#include "src/must_read.table"
};

vector<OpSet> Instruction::implicit_maybe_read_set_ {
	// Internal mnemonics
	OpSet::empty()
	// Auto-generated mnemonics
//	#include "src/maybe_read.table"
};

vector<OpSet> Instruction::implicit_must_write_set_ {
	// Internal mnemonics
	OpSet::empty()
	// Auto-generated mnemonics
//	#include "src/must_write.table"
};

vector<OpSet> Instruction::implicit_maybe_write_set_ {
	// Internal mnemonics
	OpSet::empty()
	// Auto-generated mnemonics
//	#include "src/maybe_write.table"
};

vector<OpSet> Instruction::implicit_must_undef_set_ {
	// Internal mnemonics
	OpSet::empty()
	// Auto-generated mnemonics
//	#include "src/must_undef.table"
};

vector<OpSet> Instruction::implicit_maybe_undef_set_ {
	// Internal mnemonics
	OpSet::empty()
	// Auto-generated mnemonics
//	#include "src/maybe_undef.table"
};

} // namespace x64asm
