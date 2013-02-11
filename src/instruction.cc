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

#include "src/constants.h"
#include "src/label.h"
#include "src/mm.h"
#include "src/moffs.h"
#include "src/rel.h"
#include "src/st.h"
#include "src/xmm.h"
#include "src/ymm.h"

using namespace std;

namespace {

array<const char*, 3257> att_ {{
	// Internal mnemonics
	"<label definition>"
	// Auto-generated mnemonics
	#include "src/opcode.att"
}};	

array<const char*, 3257> intel_ {{
	// Internal mnemonics
	"<label definition>"
	// Auto-generated mnemonics
	#include "src/opcode.intel"
}};	

} // namespace

namespace x64asm {

RegSet Instruction::explicit_must_read_set() const {
	auto ret = RegSet::empty();
	for ( size_t i = 0, ie = arity(); i < ie; ++i ) {
		switch ( type(i) ) {
			case OpType::M_8:
			case OpType::M_16:
			case OpType::M_32:
			case OpType::M_64:
			case OpType::M_128:
			case OpType::M_256:
			case OpType::M_16_INT:
			case OpType::M_32_INT:
			case OpType::M_64_INT:
			case OpType::M_32_FP:
			case OpType::M_64_FP:
			case OpType::M_80_FP:
			case OpType::M_80_BCD:
			case OpType::M_2_BYTE:
			case OpType::M_28_BYTE:
			case OpType::M_108_BYTE:
			case OpType::M_512_BYTE:
			case OpType::FAR_PTR_16_16:
			case OpType::FAR_PTR_16_32:
			case OpType::FAR_PTR_16_64: ret += get_operand<M8>(i); continue;

			case OpType::MOFFS_8:
			case OpType::MOFFS_16:
			case OpType::MOFFS_32:
			case OpType::MOFFS_64: ret += get_operand<Moffs8>(i); continue;

			default: break;
		}

		if ( !properties(i).contains(Property::MUST_READ) )
			continue;

		switch ( type(i) ) {
			case OpType::MM: ret += get_operand<Mm>(i); break;
			case OpType::RH: ret += get_operand<Rh>(i); break;
			case OpType::RB: ret += get_operand<Rb>(i); break;
			case OpType::AL: 
			case OpType::CL: 
			case OpType::RL: ret += get_operand<Rl>(i); break;
			case OpType::AX: 
			case OpType::DX: 
			case OpType::R_16: ret += get_operand<R16>(i); break;
			case OpType::EAX: 
			case OpType::R_32: ret += get_operand<R32>(i); break;
			case OpType::RAX: 
			case OpType::R_64: ret += get_operand<R64>(i); break;
			case OpType::FS:
			case OpType::GS: 
			case OpType::SREG: ret += get_operand<Sreg>(i); break;
			case OpType::ST_0:
			case OpType::ST: ret += get_operand<St>(i); break;
			case OpType::XMM_0:
			case OpType::XMM: ret += get_operand<Xmm>(i); break;
			case OpType::YMM: ret += get_operand<Ymm>(i); break;

			default: break;
		}
	}

	return ret;
}

RegSet Instruction::explicit_maybe_read_set() const {
	auto ret = RegSet::empty();
	for ( size_t i = 0, ie = arity(); i < ie; ++i ) {
		switch ( type(i) ) {
			case OpType::M_8:
			case OpType::M_16:
			case OpType::M_32:
			case OpType::M_64:
			case OpType::M_128:
			case OpType::M_256:
			case OpType::M_16_INT:
			case OpType::M_32_INT:
			case OpType::M_64_INT:
			case OpType::M_32_FP:
			case OpType::M_64_FP:
			case OpType::M_80_FP:
			case OpType::M_80_BCD:
			case OpType::M_2_BYTE:
			case OpType::M_28_BYTE:
			case OpType::M_108_BYTE:
			case OpType::M_512_BYTE:
			case OpType::FAR_PTR_16_16:
			case OpType::FAR_PTR_16_32:
			case OpType::FAR_PTR_16_64: ret += get_operand<M8>(i); continue;

			case OpType::MOFFS_8:
			case OpType::MOFFS_16:
			case OpType::MOFFS_32:
			case OpType::MOFFS_64: ret += get_operand<Moffs8>(i); continue;

			default: break;
		}

		if ( !properties(i).contains(Property::MAYBE_READ) )
			continue;

		switch ( type(i) ) {
			case OpType::MM: ret += get_operand<Mm>(i); break;
			case OpType::RH: ret += get_operand<Rh>(i); break;
			case OpType::RB: ret += get_operand<Rb>(i); break;
			case OpType::AL: 
			case OpType::CL: 
			case OpType::RL: ret += get_operand<Rl>(i); break;
			case OpType::AX: 
			case OpType::DX: 
			case OpType::R_16: ret += get_operand<R16>(i); break;
			case OpType::EAX: 
			case OpType::R_32: ret += get_operand<R32>(i); break;
			case OpType::RAX: 
			case OpType::R_64: ret += get_operand<R64>(i); break;
			case OpType::FS:
			case OpType::GS: 
			case OpType::SREG: ret += get_operand<Sreg>(i); break;
			case OpType::ST_0:
			case OpType::ST: ret += get_operand<St>(i); break;
			case OpType::XMM_0:
			case OpType::XMM: ret += get_operand<Xmm>(i); break;
			case OpType::YMM: ret += get_operand<Ymm>(i); break;

			default: break;
		}
	}

	return ret;
}

RegSet Instruction::explicit_must_write_set() const {
	auto ret = RegSet::empty();
	for ( size_t i = 0, ie = arity(); i < ie; ++i ) {
		if ( properties(i).contains(Property::MUST_WRITE_ZX) )
			switch ( type(i) ) {
				case OpType::EAX: 
				case OpType::R_32: ret += get_operand<R64>(i); break;
				case OpType::XMM_0:
				case OpType::XMM: ret += get_operand<Ymm>(i); break;
				default: assert(false); break;
			}
		else if ( properties(i).contains(Property::MUST_WRITE) )
			switch ( type(i) ) {
				case OpType::MM: ret += get_operand<Mm>(i); break;
				case OpType::RH: ret += get_operand<Rh>(i); break;
				case OpType::RB: ret += get_operand<Rb>(i); break;
				case OpType::AL: 
				case OpType::CL: 
				case OpType::RL: ret += get_operand<Rl>(i); break;
				case OpType::AX: 
				case OpType::DX: 
				case OpType::R_16: ret += get_operand<R16>(i); break;
				case OpType::EAX: 
				case OpType::R_32: ret += get_operand<R32>(i); break;
				case OpType::RAX: 
				case OpType::R_64: ret += get_operand<R64>(i); break;
				case OpType::FS:
				case OpType::GS: 
				case OpType::SREG: ret += get_operand<Sreg>(i); break;
				case OpType::ST_0:
				case OpType::ST: ret += get_operand<St>(i); break;
				case OpType::XMM_0:
				case OpType::XMM: ret += get_operand<Ymm>(i); break;
				case OpType::YMM: ret += get_operand<Ymm>(i); break;

				default: break;
			}
	}

	return ret;
}

RegSet Instruction::explicit_maybe_write_set() const {
	auto ret = RegSet::empty();
	for ( size_t i = 0, ie = arity(); i < ie; ++i ) {
		if ( properties(i).contains(Property::MAYBE_WRITE_ZX) )
			switch ( type(i) ) {
				case OpType::EAX: 
				case OpType::R_32: ret += get_operand<R64>(i); break;
				case OpType::XMM_0:
				case OpType::XMM: ret += get_operand<Ymm>(i); break;
				default: assert(false); break;
			}
		else if ( properties(i).contains(Property::MAYBE_WRITE) )
			switch ( type(i) ) {
				case OpType::MM: ret += get_operand<Mm>(i); break;
				case OpType::RH: ret += get_operand<Rh>(i); break;
				case OpType::RB: ret += get_operand<Rb>(i); break;
				case OpType::AL: 
				case OpType::CL: 
				case OpType::RL: ret += get_operand<Rl>(i); break;
				case OpType::AX: 
				case OpType::DX: 
				case OpType::R_16: ret += get_operand<R16>(i); break;
				case OpType::EAX: 
				case OpType::R_32: ret += get_operand<R32>(i); break;
				case OpType::RAX: 
				case OpType::R_64: ret += get_operand<R64>(i); break;
				case OpType::FS:
				case OpType::GS: 
				case OpType::SREG: ret += get_operand<Sreg>(i); break;
				case OpType::ST_0:
				case OpType::ST: ret += get_operand<St>(i); break;
				case OpType::XMM_0:
				case OpType::XMM: ret += get_operand<Ymm>(i); break;
				case OpType::YMM: ret += get_operand<Ymm>(i); break;

				default: break;
			}
	}

	return ret;
}

RegSet Instruction::explicit_must_undef_set() const {
	auto ret = RegSet::empty();
	for ( size_t i = 0, ie = arity(); i < ie; ++i )
		if ( properties(i).contains(Property::MUST_UNDEF) )
			switch ( type(i) ) {
				case OpType::MM: ret += get_operand<Mm>(i); break;
				case OpType::RH: ret += get_operand<Rh>(i); break;
				case OpType::RB: ret += get_operand<Rb>(i); break;
				case OpType::AL: 
				case OpType::CL: 
				case OpType::RL: ret += get_operand<Rl>(i); break;
				case OpType::AX: 
				case OpType::DX: 
				case OpType::R_16: ret += get_operand<R16>(i); break;
				case OpType::EAX: 
				case OpType::R_32: ret += get_operand<R32>(i); break;
				case OpType::RAX: 
				case OpType::R_64: ret += get_operand<R64>(i); break;
				case OpType::FS:
				case OpType::GS: 
				case OpType::SREG: ret += get_operand<Sreg>(i); break;
				case OpType::ST_0:
				case OpType::ST: ret += get_operand<St>(i); break;
				case OpType::XMM_0:
				case OpType::XMM: ret += get_operand<Ymm>(i); break;
				case OpType::YMM: ret += get_operand<Ymm>(i); break;

				default: break;
			}

	return ret;
}

RegSet Instruction::explicit_maybe_undef_set() const {
	auto ret = RegSet::empty();
	for ( size_t i = 0, ie = arity(); i < ie; ++i )
		if ( properties(i).contains(Property::MAYBE_UNDEF) )
			switch ( type(i) ) {
				case OpType::MM: ret += get_operand<Mm>(i); break;
				case OpType::RH: ret += get_operand<Rh>(i); break;
				case OpType::RB: ret += get_operand<Rb>(i); break;
				case OpType::AL: 
				case OpType::CL: 
				case OpType::RL: ret += get_operand<Rl>(i); break;
				case OpType::AX: 
				case OpType::DX: 
				case OpType::R_16: ret += get_operand<R16>(i); break;
				case OpType::EAX: 
				case OpType::R_32: ret += get_operand<R32>(i); break;
				case OpType::RAX: 
				case OpType::R_64: ret += get_operand<R64>(i); break;
				case OpType::FS:
				case OpType::GS: 
				case OpType::SREG: ret += get_operand<Sreg>(i); break;
				case OpType::ST_0:
				case OpType::ST: ret += get_operand<St>(i); break;
				case OpType::XMM_0:
				case OpType::XMM: ret += get_operand<Ymm>(i); break;
				case OpType::YMM: ret += get_operand<Ymm>(i); break;

				default: break;
			}

	return ret;
}

bool Instruction::check() const {
	for ( size_t i = 0, ie = arity(); i < ie; ++i )
		switch ( type(i) ) {
			case OpType::HINT: 
				if ( !get_operand<Hint>(i).check() ) return false; break;

			case OpType::IMM_8: 
				if ( !get_operand<Imm8>(i).check() ) return false; break;
			case OpType::IMM_16: 
				if ( !get_operand<Imm16>(i).check() ) return false; break;
			case OpType::IMM_32: 
				if ( !get_operand<Imm32>(i).check() ) return false; break;
			case OpType::IMM_64: 
				if ( !get_operand<Imm64>(i).check() ) return false; break;
			case OpType::ZERO: 
				if ( !get_operand<Zero>(i).check() ) return false; break;
			case OpType::ONE: 
				if ( !get_operand<One>(i).check() ) return false; break;
			case OpType::THREE: 
				if ( !get_operand<Three>(i).check() ) return false; break;

			case OpType::LABEL: 
				if ( !get_operand<Label>(i).check() ) return false; break;

			case OpType::M_8:
			case OpType::M_16:
			case OpType::M_32:
			case OpType::M_64:
			case OpType::M_128:
			case OpType::M_256:
			case OpType::M_16_INT:
			case OpType::M_32_INT:
			case OpType::M_64_INT:
			case OpType::M_32_FP:
			case OpType::M_64_FP:
			case OpType::M_80_FP:
			case OpType::M_80_BCD:
			case OpType::M_2_BYTE:
			case OpType::M_28_BYTE:
			case OpType::M_108_BYTE:
			case OpType::M_512_BYTE:
			case OpType::FAR_PTR_16_16:
			case OpType::FAR_PTR_16_32:
			case OpType::FAR_PTR_16_64: 
				if ( !get_operand<M8>(i).check() ) return false; break;

			case OpType::MM: 
				if ( !get_operand<Mm>(i).check() ) return false;  break;

			case OpType::MOFFS_8:
			case OpType::MOFFS_16:
			case OpType::MOFFS_32:
			case OpType::MOFFS_64: 
				if ( !get_operand<Moffs8>(i).check() ) return false; break;

			case OpType::PREF_66: 
				if ( !get_operand<Pref66>(i).check() ) return false; break;
			case OpType::PREF_REX_W: 
				if ( !get_operand<PrefRexW>(i).check() ) return false; break;
			case OpType::FAR: 
				if ( !get_operand<Far>(i).check() ) return false; break;

			case OpType::RH: 
				if ( !get_operand<Rh>(i).check() ) return false; break;
			case OpType::RB: 
				if ( !get_operand<Rb>(i).check() ) return false; break;
			case OpType::AL: 
				if ( !get_operand<Al>(i).check() ) return false; break;
			case OpType::CL: 
				if ( !get_operand<Cl>(i).check() ) return false; break;
			case OpType::RL: 
				if ( !get_operand<Rl>(i).check() ) return false; break;
			case OpType::AX: 
				if ( !get_operand<Ax>(i).check() ) return false; break;
			case OpType::DX: 
				if ( !get_operand<Dx>(i).check() ) return false; break;
			case OpType::R_16: 
				if ( !get_operand<R16>(i).check() ) return false; break;
			case OpType::EAX: 
				if ( !get_operand<Eax>(i).check() ) return false; break;
			case OpType::R_32: 
				if ( !get_operand<R32>(i).check() ) return false; break;
			case OpType::RAX: 
				if ( !get_operand<Rax>(i).check() ) return false; break;
			case OpType::R_64: 
				if ( !get_operand<R64>(i).check() ) return false; break;

			case OpType::REL_8: 
				if ( !get_operand<Rel8>(i).check() ) return false; break;
			case OpType::REL_32: 
				if ( !get_operand<Rel32>(i).check() ) return false; break;

			case OpType::FS: 
				if ( !get_operand<Fs>(i).check() ) return false; break;
			case OpType::GS: 
				if ( !get_operand<Gs>(i).check() ) return false; break;
			case OpType::SREG: 
				if ( !get_operand<Sreg>(i).check() ) return false; break;

			case OpType::ST_0: 
				if ( !get_operand<St0>(i).check() ) return false; break;
			case OpType::ST: 
				if ( !get_operand<St>(i).check() ) return false; break;

			case OpType::XMM_0: 
				if ( !get_operand<Xmm0>(i).check() ) return false; break;
			case OpType::XMM: 
				if ( !get_operand<Ymm>(i).check() ) return false; break;

			case OpType::YMM: 
				if ( !get_operand<Ymm>(i).check() ) return false; break;

			default: assert(false);
		}

	return true;
}

void Instruction::write_att(ostream& os) const {
	assert(get_opcode() < att_.size());
	os << att_[get_opcode()] << " ";

	if ( arity() > 0 )
		for ( int i = arity()-1; i >= 0; --i ) {
			switch ( type(i) ) {
				case OpType::HINT: 
					get_operand<Hint>(i).write_att(os); break;
				case OpType::IMM_8: 
				case OpType::IMM_16: 
				case OpType::IMM_32: 
				case OpType::IMM_64: 
				case OpType::ZERO: 
				case OpType::ONE: 
				case OpType::THREE: 
					get_operand<Three>(i).write_att(os); break;

				case OpType::LABEL: 
					get_operand<Label>(i).write_att(os); break;

				case OpType::M_8:
					get_operand<M8>(i).write_att(os); break;
				case OpType::M_16:
					get_operand<M16>(i).write_att(os); break;
				case OpType::M_32:
					get_operand<M32>(i).write_att(os); break;
				case OpType::M_64:
					get_operand<M64>(i).write_att(os); break;
				case OpType::M_128:
					get_operand<M128>(i).write_att(os); break;
				case OpType::M_256:
					get_operand<M256>(i).write_att(os); break;
				case OpType::M_16_INT:
					get_operand<M16Int>(i).write_att(os); break;
				case OpType::M_32_INT:
					get_operand<M32Int>(i).write_att(os); break;
				case OpType::M_64_INT:
					get_operand<M64Int>(i).write_att(os); break;
				case OpType::M_32_FP:
					get_operand<M32Fp>(i).write_att(os); break;
				case OpType::M_64_FP:
					get_operand<M64Fp>(i).write_att(os); break;
				case OpType::M_80_FP:
					get_operand<M80Fp>(i).write_att(os); break;
				case OpType::M_80_BCD:
					get_operand<M80Bcd>(i).write_att(os); break;
				case OpType::M_2_BYTE:
					get_operand<M2Byte>(i).write_att(os); break;
				case OpType::M_28_BYTE:
					get_operand<M28Byte>(i).write_att(os); break;
				case OpType::M_108_BYTE:
					get_operand<M108Byte>(i).write_att(os); break;
				case OpType::M_512_BYTE:
					get_operand<M512Byte>(i).write_att(os); break;
				case OpType::FAR_PTR_16_16:
					get_operand<FarPtr1616>(i).write_att(os); break;
				case OpType::FAR_PTR_16_32:
					get_operand<FarPtr1632>(i).write_att(os); break;
				case OpType::FAR_PTR_16_64: 
					get_operand<FarPtr1664>(i).write_att(os); break;

				case OpType::MM: 
					get_operand<Mm>(i).write_att(os); break;

				case OpType::MOFFS_8:
				case OpType::MOFFS_16:
				case OpType::MOFFS_32:
				case OpType::MOFFS_64: 
					get_operand<Moffs64>(i).write_att(os); break;

				case OpType::PREF_66: 
					get_operand<Pref66>(i).write_att(os); break;
				case OpType::PREF_REX_W: 
					get_operand<PrefRexW>(i).write_att(os); break;
				case OpType::FAR: 
					get_operand<Far>(i).write_att(os); break;

				case OpType::RH: 
					get_operand<Rh>(i).write_att(os); break;
				case OpType::RB: 
					get_operand<Rb>(i).write_att(os); break;
				case OpType::AL: 
				case OpType::CL: 
				case OpType::RL: 
					get_operand<Rl>(i).write_att(os); break;
				case OpType::AX: 
				case OpType::DX: 
				case OpType::R_16: 
					get_operand<R16>(i).write_att(os); break;
				case OpType::EAX: 
				case OpType::R_32: 
					get_operand<R32>(i).write_att(os); break;
				case OpType::RAX: 
				case OpType::R_64: 
					get_operand<R64>(i).write_att(os); break;

				case OpType::REL_8: 
				case OpType::REL_32: 
					get_operand<Rel32>(i).write_att(os); break;

				case OpType::FS: 
				case OpType::GS: 
				case OpType::SREG: 
					get_operand<Sreg>(i).write_att(os); break;

				case OpType::ST_0: 
				case OpType::ST: 
					get_operand<St>(i).write_att(os); break;

				case OpType::XMM_0: 
				case OpType::XMM: 
					get_operand<Xmm>(i).write_att(os); break;

				case OpType::YMM: 
					get_operand<Ymm>(i).write_att(os); break;

				default: assert(false);
			}

			if ( i != 0 )
				os << ", ";
		}
}

void Instruction::write_intel(ostream& os) const {
	assert(get_opcode() < intel_.size());
	os << intel_[get_opcode()] << " ";

	for ( size_t i = 0, ie = arity(); i < ie; ++i ) {
		switch ( type(i) ) {
			case OpType::HINT: 
				get_operand<Hint>(i).write_att(os); break;
			case OpType::IMM_8: 
			case OpType::IMM_16: 
			case OpType::IMM_32: 
			case OpType::IMM_64: 
			case OpType::ZERO: 
			case OpType::ONE: 
			case OpType::THREE: 
				get_operand<Three>(i).write_att(os); break;

			case OpType::LABEL: 
				get_operand<Label>(i).write_att(os); break;

			case OpType::M_8:
				get_operand<M8>(i).write_att(os); break;
			case OpType::M_16:
				get_operand<M16>(i).write_att(os); break;
			case OpType::M_32:
				get_operand<M32>(i).write_att(os); break;
			case OpType::M_64:
				get_operand<M64>(i).write_att(os); break;
			case OpType::M_128:
				get_operand<M128>(i).write_att(os); break;
			case OpType::M_256:
				get_operand<M256>(i).write_att(os); break;
			case OpType::M_16_INT:
				get_operand<M16Int>(i).write_att(os); break;
			case OpType::M_32_INT:
				get_operand<M32Int>(i).write_att(os); break;
			case OpType::M_64_INT:
				get_operand<M64Int>(i).write_att(os); break;
			case OpType::M_32_FP:
				get_operand<M32Fp>(i).write_att(os); break;
			case OpType::M_64_FP:
				get_operand<M64Fp>(i).write_att(os); break;
			case OpType::M_80_FP:
				get_operand<M80Fp>(i).write_att(os); break;
			case OpType::M_80_BCD:
				get_operand<M80Bcd>(i).write_att(os); break;
			case OpType::M_2_BYTE:
				get_operand<M2Byte>(i).write_att(os); break;
			case OpType::M_28_BYTE:
				get_operand<M28Byte>(i).write_att(os); break;
			case OpType::M_108_BYTE:
				get_operand<M108Byte>(i).write_att(os); break;
			case OpType::M_512_BYTE:
				get_operand<M512Byte>(i).write_att(os); break;
			case OpType::FAR_PTR_16_16:
				get_operand<FarPtr1616>(i).write_att(os); break;
			case OpType::FAR_PTR_16_32:
				get_operand<FarPtr1632>(i).write_att(os); break;
			case OpType::FAR_PTR_16_64: 
				get_operand<FarPtr1664>(i).write_att(os); break;

			case OpType::MM: 
				get_operand<Mm>(i).write_att(os); break;

			case OpType::MOFFS_8:
			case OpType::MOFFS_16:
			case OpType::MOFFS_32:
			case OpType::MOFFS_64: 
				get_operand<Moffs64>(i).write_att(os); break;

			case OpType::PREF_66: 
				get_operand<Pref66>(i).write_att(os); break;
			case OpType::PREF_REX_W: 
				get_operand<PrefRexW>(i).write_att(os); break;
			case OpType::FAR: 
				get_operand<Far>(i).write_att(os); break;

			case OpType::RH: 
				get_operand<Rh>(i).write_att(os); break;
			case OpType::RB: 
				get_operand<Rb>(i).write_att(os); break;
			case OpType::AL: 
			case OpType::CL: 
			case OpType::RL: 
				get_operand<Rl>(i).write_att(os); break;
			case OpType::AX: 
			case OpType::DX: 
			case OpType::R_16: 
				get_operand<R16>(i).write_att(os); break;
			case OpType::EAX: 
			case OpType::R_32: 
				get_operand<R32>(i).write_att(os); break;
			case OpType::RAX: 
			case OpType::R_64: 
				get_operand<R64>(i).write_att(os); break;

			case OpType::REL_8: 
			case OpType::REL_32: 
				get_operand<Rel32>(i).write_att(os); break;

			case OpType::FS: 
			case OpType::GS: 
			case OpType::SREG: 
				get_operand<Sreg>(i).write_att(os); break;

			case OpType::ST_0: 
			case OpType::ST: 
				get_operand<St>(i).write_att(os); break;

			case OpType::XMM_0: 
			case OpType::XMM: 
				get_operand<Xmm>(i).write_att(os); break;

			case OpType::YMM: 
				get_operand<Ymm>(i).write_att(os); break;

			default: assert(false);
		}

		if ( (i+1) != ie )
			os << ", ";
	}
}

const array<size_t, 3257> Instruction::arity_ {{
	// Internal mnemonics
	0
	// Auto-generated mnemonics
	#include "src/arity.table"
}};

const array<array<Properties, 4>, 3257> Instruction::properties_ {{
	// Internal mnemonics
	{{}}
	// Auto-generated mnemonics
	#include "src/properties.table"
}};

const array<array<OpType, 4>, 3257> Instruction::type_ {{
	// Internal mnemonics
	{{}}
	// Auto-generated mnemonics
	#include "src/type.table"
}};

const array<bool, 3257> Instruction::is_return_ {{
	// Internal mnemonics
	false
	// Auto-generated mnemonics
	#include "src/return.table"
}};

const array<bool, 3257> Instruction::is_nop_ {{
	// Internal mnemonics
	false
	// Auto-generated mnemonics
	#include "src/nop.table"
}};

const array<bool, 3257> Instruction::is_jump_ {{
	// Internal mnemonics
	false
	// Auto-generated mnemonics
	#include "src/jump.table"
}};

const array<bool, 3257> Instruction::is_cond_jump_ {{
	// Internal mnemonics
	false
	// Auto-generated mnemonics
	#include "src/cond_jump.table"
}};

const array<bool, 3257> Instruction::is_uncond_jump_ {{
	// Internal mnemonics
	false
	// Auto-generated mnemonics
	#include "src/uncond_jump.table"
}};

const array<RegSet, 3257> Instruction::implicit_must_read_set_ {{
	// Internal mnemonics
	RegSet::empty()
	// Auto-generated mnemonics
	#include "src/must_read.table"
}};

const array<RegSet, 3257> Instruction::implicit_maybe_read_set_ {{
	// Internal mnemonics
	RegSet::empty()
	// Auto-generated mnemonics
	#include "src/maybe_read.table"
}};

const array<RegSet, 3257> Instruction::implicit_must_write_set_ {{
	// Internal mnemonics
	RegSet::empty()
	// Auto-generated mnemonics
	#include "src/must_write.table"
}};

const array<RegSet, 3257> Instruction::implicit_maybe_write_set_ {{
	// Internal mnemonics
	RegSet::empty()
	// Auto-generated mnemonics
	#include "src/maybe_write.table"
}};

const array<RegSet, 3257> Instruction::implicit_must_undef_set_ {{
	// Internal mnemonics
	RegSet::empty()
	// Auto-generated mnemonics
	#include "src/must_undef.table"
}};

const array<RegSet, 3257> Instruction::implicit_maybe_undef_set_ {{
	// Internal mnemonics
	RegSet::empty()
	// Auto-generated mnemonics
	#include "src/maybe_undef.table"
}};

} // namespace x64asm
