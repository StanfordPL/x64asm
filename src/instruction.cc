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

RegSet& Instruction::explicit_must_read_set(RegSet& ret) const {
	for ( size_t i = 0, ie = arity(); i < ie; ++i ) {
		switch ( type(i) ) {
			case Type::M_8:
			case Type::M_16:
			case Type::M_32:
			case Type::M_64:
			case Type::M_128:
			case Type::M_256:
			case Type::M_16_INT:
			case Type::M_32_INT:
			case Type::M_64_INT:
			case Type::M_32_FP:
			case Type::M_64_FP:
			case Type::M_80_FP:
			case Type::M_80_BCD:
			case Type::M_2_BYTE:
			case Type::M_28_BYTE:
			case Type::M_108_BYTE:
			case Type::M_512_BYTE:
			case Type::FAR_PTR_16_16:
			case Type::FAR_PTR_16_32:
			case Type::FAR_PTR_16_64: ret += get_operand<M8>(i); continue;

			case Type::MOFFS_8:
			case Type::MOFFS_16:
			case Type::MOFFS_32:
			case Type::MOFFS_64: ret += get_operand<Moffs8>(i); continue;

			default: break;
		}

		if ( !must_read(i) )
			continue;

		switch ( type(i) ) {
			case Type::MM: ret += get_operand<Mm>(i); break;
			case Type::RH: ret += get_operand<Rh>(i); break;
			case Type::RB: ret += get_operand<Rb>(i); break;
			case Type::AL: 
			case Type::CL: 
			case Type::RL: ret += get_operand<Rl>(i); break;
			case Type::AX: 
			case Type::DX: 
			case Type::R_16: ret += get_operand<R16>(i); break;
			case Type::EAX: 
			case Type::R_32: ret += get_operand<R32>(i); break;
			case Type::RAX: 
			case Type::R_64: ret += get_operand<R64>(i); break;
			case Type::FS:
			case Type::GS: 
			case Type::SREG: ret += get_operand<Sreg>(i); break;
			case Type::ST_0:
			case Type::ST: ret += get_operand<St>(i); break;
			case Type::XMM_0:
			case Type::XMM: ret += get_operand<Xmm>(i); break;
			case Type::YMM: ret += get_operand<Ymm>(i); break;

			default: break;
		}
	}

	return ret;
}

RegSet& Instruction::explicit_maybe_read_set(RegSet& ret) const {
	for ( size_t i = 0, ie = arity(); i < ie; ++i ) {
		switch ( type(i) ) {
			case Type::M_8:
			case Type::M_16:
			case Type::M_32:
			case Type::M_64:
			case Type::M_128:
			case Type::M_256:
			case Type::M_16_INT:
			case Type::M_32_INT:
			case Type::M_64_INT:
			case Type::M_32_FP:
			case Type::M_64_FP:
			case Type::M_80_FP:
			case Type::M_80_BCD:
			case Type::M_2_BYTE:
			case Type::M_28_BYTE:
			case Type::M_108_BYTE:
			case Type::M_512_BYTE:
			case Type::FAR_PTR_16_16:
			case Type::FAR_PTR_16_32:
			case Type::FAR_PTR_16_64: ret += get_operand<M8>(i); continue;

			case Type::MOFFS_8:
			case Type::MOFFS_16:
			case Type::MOFFS_32:
			case Type::MOFFS_64: ret += get_operand<Moffs8>(i); continue;

			default: break;
		}

		if ( !maybe_read(i) )
			continue;

		switch ( type(i) ) {
			case Type::MM: ret += get_operand<Mm>(i); break;
			case Type::RH: ret += get_operand<Rh>(i); break;
			case Type::RB: ret += get_operand<Rb>(i); break;
			case Type::AL: 
			case Type::CL: 
			case Type::RL: ret += get_operand<Rl>(i); break;
			case Type::AX: 
			case Type::DX: 
			case Type::R_16: ret += get_operand<R16>(i); break;
			case Type::EAX: 
			case Type::R_32: ret += get_operand<R32>(i); break;
			case Type::RAX: 
			case Type::R_64: ret += get_operand<R64>(i); break;
			case Type::FS:
			case Type::GS: 
			case Type::SREG: ret += get_operand<Sreg>(i); break;
			case Type::ST_0:
			case Type::ST: ret += get_operand<St>(i); break;
			case Type::XMM_0:
			case Type::XMM: ret += get_operand<Xmm>(i); break;
			case Type::YMM: ret += get_operand<Ymm>(i); break;

			default: break;
		}
	}

	return ret;
}

RegSet& Instruction::explicit_must_write_set(RegSet& ret) const {
	for ( size_t i = 0, ie = arity(); i < ie; ++i ) {
		if ( must_extend(i) ) 
			switch ( type(i) ) {
				case Type::EAX: 
				case Type::R_32: ret += get_operand<R64>(i); break;
				case Type::XMM_0:
				case Type::XMM: ret += get_operand<Ymm>(i); break;
				default: assert(false); break;
			}
		else if ( must_write(i) ) 
			switch ( type(i) ) {
				case Type::MM: ret += get_operand<Mm>(i); break;
				case Type::RH: ret += get_operand<Rh>(i); break;
				case Type::RB: ret += get_operand<Rb>(i); break;
				case Type::AL: 
				case Type::CL: 
				case Type::RL: ret += get_operand<Rl>(i); break;
				case Type::AX: 
				case Type::DX: 
				case Type::R_16: ret += get_operand<R16>(i); break;
				case Type::EAX: 
				case Type::R_32: ret += get_operand<R32>(i); break;
				case Type::RAX: 
				case Type::R_64: ret += get_operand<R64>(i); break;
				case Type::FS:
				case Type::GS: 
				case Type::SREG: ret += get_operand<Sreg>(i); break;
				case Type::ST_0:
				case Type::ST: ret += get_operand<St>(i); break;
				case Type::XMM_0:
				case Type::XMM: ret += get_operand<Ymm>(i); break;
				case Type::YMM: ret += get_operand<Ymm>(i); break;

				default: break;
			}
	}

	return ret;
}

RegSet& Instruction::explicit_maybe_write_set(RegSet& ret) const {
	for ( size_t i = 0, ie = arity(); i < ie; ++i ) {
		if ( maybe_extend(i) ) 
			switch ( type(i) ) {
				case Type::EAX: 
				case Type::R_32: ret += get_operand<R64>(i); break;
				case Type::XMM_0:
				case Type::XMM: ret += get_operand<Ymm>(i); break;
				default: assert(false); break;
			}
		else if ( maybe_write(i) )
			switch ( type(i) ) {
				case Type::MM: ret += get_operand<Mm>(i); break;
				case Type::RH: ret += get_operand<Rh>(i); break;
				case Type::RB: ret += get_operand<Rb>(i); break;
				case Type::AL: 
				case Type::CL: 
				case Type::RL: ret += get_operand<Rl>(i); break;
				case Type::AX: 
				case Type::DX: 
				case Type::R_16: ret += get_operand<R16>(i); break;
				case Type::EAX: 
				case Type::R_32: ret += get_operand<R32>(i); break;
				case Type::RAX: 
				case Type::R_64: ret += get_operand<R64>(i); break;
				case Type::FS:
				case Type::GS: 
				case Type::SREG: ret += get_operand<Sreg>(i); break;
				case Type::ST_0:
				case Type::ST: ret += get_operand<St>(i); break;
				case Type::XMM_0:
				case Type::XMM: ret += get_operand<Ymm>(i); break;
				case Type::YMM: ret += get_operand<Ymm>(i); break;

				default: break;
			}
	}

	return ret;
}

RegSet& Instruction::explicit_must_undef_set(RegSet& ret) const {
	for ( size_t i = 0, ie = arity(); i < ie; ++i )
		if ( must_undef(i) )
			switch ( type(i) ) {
				case Type::MM: ret += get_operand<Mm>(i); break;
				case Type::RH: ret += get_operand<Rh>(i); break;
				case Type::RB: ret += get_operand<Rb>(i); break;
				case Type::AL: 
				case Type::CL: 
				case Type::RL: ret += get_operand<Rl>(i); break;
				case Type::AX: 
				case Type::DX: 
				case Type::R_16: ret += get_operand<R16>(i); break;
				case Type::EAX: 
				case Type::R_32: ret += get_operand<R32>(i); break;
				case Type::RAX: 
				case Type::R_64: ret += get_operand<R64>(i); break;
				case Type::FS:
				case Type::GS: 
				case Type::SREG: ret += get_operand<Sreg>(i); break;
				case Type::ST_0:
				case Type::ST: ret += get_operand<St>(i); break;
				case Type::XMM_0:
				case Type::XMM: ret += get_operand<Ymm>(i); break;
				case Type::YMM: ret += get_operand<Ymm>(i); break;

				default: break;
			}

	return ret;
}

RegSet& Instruction::explicit_maybe_undef_set(RegSet& ret) const {
	for ( size_t i = 0, ie = arity(); i < ie; ++i )
		if ( maybe_undef(i) )
			switch ( type(i) ) {
				case Type::MM: ret += get_operand<Mm>(i); break;
				case Type::RH: ret += get_operand<Rh>(i); break;
				case Type::RB: ret += get_operand<Rb>(i); break;
				case Type::AL: 
				case Type::CL: 
				case Type::RL: ret += get_operand<Rl>(i); break;
				case Type::AX: 
				case Type::DX: 
				case Type::R_16: ret += get_operand<R16>(i); break;
				case Type::EAX: 
				case Type::R_32: ret += get_operand<R32>(i); break;
				case Type::RAX: 
				case Type::R_64: ret += get_operand<R64>(i); break;
				case Type::FS:
				case Type::GS: 
				case Type::SREG: ret += get_operand<Sreg>(i); break;
				case Type::ST_0:
				case Type::ST: ret += get_operand<St>(i); break;
				case Type::XMM_0:
				case Type::XMM: ret += get_operand<Ymm>(i); break;
				case Type::YMM: ret += get_operand<Ymm>(i); break;

				default: break;
			}

	return ret;
}

bool Instruction::check() const {
	for ( size_t i = 0, ie = arity(); i < ie; ++i )
		switch ( type(i) ) {
			case Type::HINT: 
				if ( !get_operand<Hint>(i).check() ) return false; break;

			case Type::IMM_8: 
				if ( !get_operand<Imm8>(i).check() ) return false; break;
			case Type::IMM_16: 
				if ( !get_operand<Imm16>(i).check() ) return false; break;
			case Type::IMM_32: 
				if ( !get_operand<Imm32>(i).check() ) return false; break;
			case Type::IMM_64: 
				if ( !get_operand<Imm64>(i).check() ) return false; break;
			case Type::ZERO: 
				if ( !get_operand<Zero>(i).check() ) return false; break;
			case Type::ONE: 
				if ( !get_operand<One>(i).check() ) return false; break;
			case Type::THREE: 
				if ( !get_operand<Three>(i).check() ) return false; break;

			case Type::LABEL: 
				if ( !get_operand<Label>(i).check() ) return false; break;

			case Type::M_8:
			case Type::M_16:
			case Type::M_32:
			case Type::M_64:
			case Type::M_128:
			case Type::M_256:
			case Type::M_16_INT:
			case Type::M_32_INT:
			case Type::M_64_INT:
			case Type::M_32_FP:
			case Type::M_64_FP:
			case Type::M_80_FP:
			case Type::M_80_BCD:
			case Type::M_2_BYTE:
			case Type::M_28_BYTE:
			case Type::M_108_BYTE:
			case Type::M_512_BYTE:
			case Type::FAR_PTR_16_16:
			case Type::FAR_PTR_16_32:
			case Type::FAR_PTR_16_64: 
				if ( !get_operand<M8>(i).check() ) return false; break;

			case Type::MM: 
				if ( !get_operand<Mm>(i).check() ) return false;  break;

			case Type::MOFFS_8:
			case Type::MOFFS_16:
			case Type::MOFFS_32:
			case Type::MOFFS_64: 
				if ( !get_operand<Moffs8>(i).check() ) return false; break;

			case Type::PREF_66: 
				if ( !get_operand<Pref66>(i).check() ) return false; break;
			case Type::PREF_REX_W: 
				if ( !get_operand<PrefRexW>(i).check() ) return false; break;
			case Type::FAR: 
				if ( !get_operand<Far>(i).check() ) return false; break;

			case Type::RH: 
				if ( !get_operand<Rh>(i).check() ) return false; break;
			case Type::RB: 
				if ( !get_operand<Rb>(i).check() ) return false; break;
			case Type::AL: 
				if ( !get_operand<Al>(i).check() ) return false; break;
			case Type::CL: 
				if ( !get_operand<Cl>(i).check() ) return false; break;
			case Type::RL: 
				if ( !get_operand<Rl>(i).check() ) return false; break;
			case Type::AX: 
				if ( !get_operand<Ax>(i).check() ) return false; break;
			case Type::DX: 
				if ( !get_operand<Dx>(i).check() ) return false; break;
			case Type::R_16: 
				if ( !get_operand<R16>(i).check() ) return false; break;
			case Type::EAX: 
				if ( !get_operand<Eax>(i).check() ) return false; break;
			case Type::R_32: 
				if ( !get_operand<R32>(i).check() ) return false; break;
			case Type::RAX: 
				if ( !get_operand<Rax>(i).check() ) return false; break;
			case Type::R_64: 
				if ( !get_operand<R64>(i).check() ) return false; break;

			case Type::REL_8: 
				if ( !get_operand<Rel8>(i).check() ) return false; break;
			case Type::REL_32: 
				if ( !get_operand<Rel32>(i).check() ) return false; break;

			case Type::FS: 
				if ( !get_operand<Fs>(i).check() ) return false; break;
			case Type::GS: 
				if ( !get_operand<Gs>(i).check() ) return false; break;
			case Type::SREG: 
				if ( !get_operand<Sreg>(i).check() ) return false; break;

			case Type::ST_0: 
				if ( !get_operand<St0>(i).check() ) return false; break;
			case Type::ST: 
				if ( !get_operand<St>(i).check() ) return false; break;

			case Type::XMM_0: 
				if ( !get_operand<Xmm0>(i).check() ) return false; break;
			case Type::XMM: 
				if ( !get_operand<Ymm>(i).check() ) return false; break;

			case Type::YMM: 
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
				case Type::HINT: 
					get_operand<Hint>(i).write_att(os); break;
				case Type::IMM_8: 
				case Type::IMM_16: 
				case Type::IMM_32: 
				case Type::IMM_64: 
				case Type::ZERO: 
				case Type::ONE: 
				case Type::THREE: 
					get_operand<Three>(i).write_att(os); break;

				case Type::LABEL: 
					get_operand<Label>(i).write_att(os); break;

				case Type::M_8:
					get_operand<M8>(i).write_att(os); break;
				case Type::M_16:
					get_operand<M16>(i).write_att(os); break;
				case Type::M_32:
					get_operand<M32>(i).write_att(os); break;
				case Type::M_64:
					get_operand<M64>(i).write_att(os); break;
				case Type::M_128:
					get_operand<M128>(i).write_att(os); break;
				case Type::M_256:
					get_operand<M256>(i).write_att(os); break;
				case Type::M_16_INT:
					get_operand<M16Int>(i).write_att(os); break;
				case Type::M_32_INT:
					get_operand<M32Int>(i).write_att(os); break;
				case Type::M_64_INT:
					get_operand<M64Int>(i).write_att(os); break;
				case Type::M_32_FP:
					get_operand<M32Fp>(i).write_att(os); break;
				case Type::M_64_FP:
					get_operand<M64Fp>(i).write_att(os); break;
				case Type::M_80_FP:
					get_operand<M80Fp>(i).write_att(os); break;
				case Type::M_80_BCD:
					get_operand<M80Bcd>(i).write_att(os); break;
				case Type::M_2_BYTE:
					get_operand<M2Byte>(i).write_att(os); break;
				case Type::M_28_BYTE:
					get_operand<M28Byte>(i).write_att(os); break;
				case Type::M_108_BYTE:
					get_operand<M108Byte>(i).write_att(os); break;
				case Type::M_512_BYTE:
					get_operand<M512Byte>(i).write_att(os); break;
				case Type::FAR_PTR_16_16:
					get_operand<FarPtr1616>(i).write_att(os); break;
				case Type::FAR_PTR_16_32:
					get_operand<FarPtr1632>(i).write_att(os); break;
				case Type::FAR_PTR_16_64: 
					get_operand<FarPtr1664>(i).write_att(os); break;

				case Type::MM: 
					get_operand<Mm>(i).write_att(os); break;

				case Type::MOFFS_8:
				case Type::MOFFS_16:
				case Type::MOFFS_32:
				case Type::MOFFS_64: 
					get_operand<Moffs64>(i).write_att(os); break;

				case Type::PREF_66: 
					get_operand<Pref66>(i).write_att(os); break;
				case Type::PREF_REX_W: 
					get_operand<PrefRexW>(i).write_att(os); break;
				case Type::FAR: 
					get_operand<Far>(i).write_att(os); break;

				case Type::RH: 
					get_operand<Rh>(i).write_att(os); break;
				case Type::RB: 
					get_operand<Rb>(i).write_att(os); break;
				case Type::AL: 
				case Type::CL: 
				case Type::RL: 
					get_operand<Rl>(i).write_att(os); break;
				case Type::AX: 
				case Type::DX: 
				case Type::R_16: 
					get_operand<R16>(i).write_att(os); break;
				case Type::EAX: 
				case Type::R_32: 
					get_operand<R32>(i).write_att(os); break;
				case Type::RAX: 
				case Type::R_64: 
					get_operand<R64>(i).write_att(os); break;

				case Type::REL_8: 
				case Type::REL_32: 
					get_operand<Rel32>(i).write_att(os); break;

				case Type::FS: 
				case Type::GS: 
				case Type::SREG: 
					get_operand<Sreg>(i).write_att(os); break;

				case Type::ST_0: 
				case Type::ST: 
					get_operand<St>(i).write_att(os); break;

				case Type::XMM_0: 
				case Type::XMM: 
					get_operand<Xmm>(i).write_att(os); break;

				case Type::YMM: 
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
			case Type::HINT: 
				get_operand<Hint>(i).write_att(os); break;
			case Type::IMM_8: 
			case Type::IMM_16: 
			case Type::IMM_32: 
			case Type::IMM_64: 
			case Type::ZERO: 
			case Type::ONE: 
			case Type::THREE: 
				get_operand<Three>(i).write_att(os); break;

			case Type::LABEL: 
				get_operand<Label>(i).write_att(os); break;

			case Type::M_8:
				get_operand<M8>(i).write_att(os); break;
			case Type::M_16:
				get_operand<M16>(i).write_att(os); break;
			case Type::M_32:
				get_operand<M32>(i).write_att(os); break;
			case Type::M_64:
				get_operand<M64>(i).write_att(os); break;
			case Type::M_128:
				get_operand<M128>(i).write_att(os); break;
			case Type::M_256:
				get_operand<M256>(i).write_att(os); break;
			case Type::M_16_INT:
				get_operand<M16Int>(i).write_att(os); break;
			case Type::M_32_INT:
				get_operand<M32Int>(i).write_att(os); break;
			case Type::M_64_INT:
				get_operand<M64Int>(i).write_att(os); break;
			case Type::M_32_FP:
				get_operand<M32Fp>(i).write_att(os); break;
			case Type::M_64_FP:
				get_operand<M64Fp>(i).write_att(os); break;
			case Type::M_80_FP:
				get_operand<M80Fp>(i).write_att(os); break;
			case Type::M_80_BCD:
				get_operand<M80Bcd>(i).write_att(os); break;
			case Type::M_2_BYTE:
				get_operand<M2Byte>(i).write_att(os); break;
			case Type::M_28_BYTE:
				get_operand<M28Byte>(i).write_att(os); break;
			case Type::M_108_BYTE:
				get_operand<M108Byte>(i).write_att(os); break;
			case Type::M_512_BYTE:
				get_operand<M512Byte>(i).write_att(os); break;
			case Type::FAR_PTR_16_16:
				get_operand<FarPtr1616>(i).write_att(os); break;
			case Type::FAR_PTR_16_32:
				get_operand<FarPtr1632>(i).write_att(os); break;
			case Type::FAR_PTR_16_64: 
				get_operand<FarPtr1664>(i).write_att(os); break;

			case Type::MM: 
				get_operand<Mm>(i).write_att(os); break;

			case Type::MOFFS_8:
			case Type::MOFFS_16:
			case Type::MOFFS_32:
			case Type::MOFFS_64: 
				get_operand<Moffs64>(i).write_att(os); break;

			case Type::PREF_66: 
				get_operand<Pref66>(i).write_att(os); break;
			case Type::PREF_REX_W: 
				get_operand<PrefRexW>(i).write_att(os); break;
			case Type::FAR: 
				get_operand<Far>(i).write_att(os); break;

			case Type::RH: 
				get_operand<Rh>(i).write_att(os); break;
			case Type::RB: 
				get_operand<Rb>(i).write_att(os); break;
			case Type::AL: 
			case Type::CL: 
			case Type::RL: 
				get_operand<Rl>(i).write_att(os); break;
			case Type::AX: 
			case Type::DX: 
			case Type::R_16: 
				get_operand<R16>(i).write_att(os); break;
			case Type::EAX: 
			case Type::R_32: 
				get_operand<R32>(i).write_att(os); break;
			case Type::RAX: 
			case Type::R_64: 
				get_operand<R64>(i).write_att(os); break;

			case Type::REL_8: 
			case Type::REL_32: 
				get_operand<Rel32>(i).write_att(os); break;

			case Type::FS: 
			case Type::GS: 
			case Type::SREG: 
				get_operand<Sreg>(i).write_att(os); break;

			case Type::ST_0: 
			case Type::ST: 
				get_operand<St>(i).write_att(os); break;

			case Type::XMM_0: 
			case Type::XMM: 
				get_operand<Xmm>(i).write_att(os); break;

			case Type::YMM: 
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

const array<array<Instruction::Properties, 4>, 3257> Instruction::properties_ {{
	// Internal mnemonics
	{{}}
	// Auto-generated mnemonics
	#include "src/properties.table"
}};

const array<array<Type, 4>, 3257> Instruction::type_ {{
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
