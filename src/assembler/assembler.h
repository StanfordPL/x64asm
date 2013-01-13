#ifndef X64_SRC_ASSEMBLER_ASSEMBLER_H
#define X64_SRC_ASSEMBLER_ASSEMBLER_H

#include <iostream>
#include <unordered_map>

#include "src/assembler/function.h"
#include "src/code/code.h"
#include "src/code/cr.h"
#include "src/code/dr.h"
#include "src/code/hint.h"
#include "src/code/imm.h"
#include "src/code/instruction.h"
#include "src/code/label.h"
#include "src/code/m.h"
#include "src/code/mm.h"
#include "src/code/modifier.h"
#include "src/code/moffs.h"
#include "src/code/r.h"
#include "src/code/rel.h"
#include "src/code/sreg.h"
#include "src/code/st.h"
#include "src/code/xmm.h"
#include "src/code/ymm.h"

namespace x64 {

/** An x64 assembler. */
class Assembler {
	public:
		inline void assemble(Function& fxn, const Code& code) {
			start(fxn);
			for ( const auto& instr : code )
				assemble(instr);
			finish();
		}

		void assemble(const Instruction& instr);

		void start(Function& fxn) {
			fxn_ = &fxn;
			fxn_->clear();

			label_defs_.clear();
			label_rels_.clear();
		}	

		void finish() { 
			for ( const auto& l : label_rels_ ) {
				const auto pos = l.first;
				const auto itr = label_defs_.find(l.second);

				if ( itr == label_defs_.end() ) 
					fxn_->emit_long(0, pos);
				else
					fxn_->emit_long(itr->second-pos-4, pos);
			}
		}

		#include "src/assembler/assembler.decl"

		inline void bind(Label label) {
			label_defs_[label.val_] = fxn_->size();
		}

	private:
		Function* fxn_;
		
		std::unordered_map<uint64_t, size_t> label_defs_;
		std::vector<std::pair<size_t, uint64_t>> label_rels_;

		inline void pref_group1(uint8_t c) {
			fxn_->emit_byte(c);
		}

		inline void pref_group2(const M m) {
			static uint8_t pref[6] {0x26, 0x2e, 0x36, 0x3e, 0x64, 0x65};
			if ( !m.null_seg() )
				fxn_->emit_byte(pref[m.get_seg().val_]);
		}

		inline void pref_group2(const Hint h) {
			fxn_->emit_byte(h == Hint::TAKEN ? 0x3e : 0x2e);
		}

		inline void pref_group3() {
			fxn_->emit_byte(0x66);
		}

		inline void pref_group4(const M m) {
			if ( m.get_addr_or() )
				fxn_->emit_byte(0x67);
		}

		inline void opcode(uint8_t o1) {
			fxn_->emit_byte(o1);
		}

		inline void opcode(uint8_t o1, Operand rcode) {
			const auto delta = rcode.val_ & 0x7;
			fxn_->emit_byte(o1 + delta);
		}

		inline void opcode(uint8_t o1, uint8_t o2) {
			fxn_->emit_byte(o1);
			fxn_->emit_byte(o2);
		}

		inline void opcode(uint8_t o1, uint8_t o2, uint8_t o3) {
			fxn_->emit_byte(o1);
			fxn_->emit_byte(o2);
			fxn_->emit_byte(o3);
		}

		inline void disp_imm(Imm8 i) {
			fxn_->emit_byte(i.val_);
		}

		inline void disp_imm(Imm16 i) {
			fxn_->emit_word(i.val_);
		}

		inline void disp_imm(Imm32 i) {
			fxn_->emit_long(i.val_);
		}

		inline void disp_imm(Imm64 i) {
			fxn_->emit_quad(i.val_);
		}

		inline void disp_imm(Moffs8 m) {
			fxn_->emit_quad(m.val_);
		}

		inline void disp_imm(Moffs16 m) {
			fxn_->emit_quad(m.val_);
		}

		inline void disp_imm(Moffs32 m) {
			fxn_->emit_quad(m.val_);
		}

		inline void disp_imm(Moffs64 m) {
			fxn_->emit_quad(m.val_);
		}

		inline void disp_imm(Rel8 r) {
			fxn_->emit_byte(r.val_);
		}

		inline void disp_imm(Rel32 r) {
			fxn_->emit_long(r.val_);
		}

		inline void disp_imm(Label l) {
			label_rels_.push_back(std::make_pair(fxn_->size(), l.val_));
			fxn_->advance_long();
		}

		inline void resize() {
			if ( fxn_->capacity() - fxn_->size() < 15 ) 
				fxn_->resize(fxn_->capacity()*2);
		}

};

} // namespace x64

#endif
