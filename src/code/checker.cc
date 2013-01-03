#include "src/code/checker.h"

#include "src/code/attributes.h"

namespace x64 {

bool Checker::check(const Instruction& instr) {
	for ( size_t i = 0, ie = Attributes::arity(instr); i < ie; ++i ) {
		const auto o = instr.get_operand(i);

		#define CHECK(T) if ( !check((T)o) ) return false; break;
		switch ( Attributes::type(instr, i) ) {
			case OpType::CR:         CHECK(Cr);
			case OpType::CR_0234:    CHECK(Cr0234);
			case OpType::CR_8:       CHECK(Cr8);
			case OpType::DR:         CHECK(Dr);
			case OpType::EFLAG:      CHECK(Eflag);
			case OpType::IMM:        CHECK(Imm);
			case OpType::IMM_8:      CHECK(Imm8);
			case OpType::IMM_16:     CHECK(Imm16);
			case OpType::IMM_32:     CHECK(Imm32);
			case OpType::IMM_64:     CHECK(Imm64);
			case OpType::ZERO:       CHECK(Zero);
			case OpType::ONE:        CHECK(One);
			case OpType::THREE:      CHECK(Three);
			case OpType::LABEL:      CHECK(Label);
			case OpType::M:
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
			case OpType::M_512_BYTE: CHECK(M);
			case OpType::MM:         CHECK(Mm);
			case OpType::MODIFIER:   CHECK(Modifier);
			case OpType::PREF_66:    CHECK(Pref66);
			case OpType::PREF_REX_W: CHECK(PrefRexW);
			case OpType::FAR:        CHECK(Far);
			case OpType::MOFFS:      
			case OpType::MOFFS_8:
			case OpType::MOFFS_16:
			case OpType::MOFFS_32:
			case OpType::MOFFS_64:   CHECK(Moffs);
			case OpType::R:          CHECK(R);
			case OpType::NO_REX_R8:  CHECK(NoRexR8);
			case OpType::REX_R8:     CHECK(RexR8);
			case OpType::RH:         CHECK(Rh);
			case OpType::RL:         CHECK(Rl);
			case OpType::AL:         CHECK(Al);
			case OpType::CL:         CHECK(Cl);
			case OpType::RB:         CHECK(Rb);
			case OpType::R_16:       CHECK(R16); 
			case OpType::AX:         CHECK(Ax);
			case OpType::DX:         CHECK(Dx);
			case OpType::R_32:       CHECK(R32);
			case OpType::EAX:        CHECK(Eax);
			case OpType::R_64:       CHECK(R64);
			case OpType::RAX:        CHECK(Rax);
			case OpType::REL:        CHECK(Rel);
			case OpType::REL_8:      CHECK(Rel8);
			case OpType::REL_32:     CHECK(Rel32);
			case OpType::SREG:       CHECK(Sreg);
			case OpType::FS:         CHECK(Fs);
			case OpType::GS:         CHECK(Gs);
			case OpType::XMM:        CHECK(Xmm);
			case OpType::XMM_0:      CHECK(Xmm0);
			case OpType::YMM:        CHECK(Ymm);
			default: assert(false);
		}
		#undef CHECK
	}
	return true;
}

} // namespace x64
