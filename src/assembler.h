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

#ifndef X64ASM_SRC_ASSEMBLER_H
#define X64ASM_SRC_ASSEMBLER_H

#include <iostream>
#include <type_traits>
#include <unordered_map>

#include "src/function.h"
#include "src/code.h"
#include "src/hint.h"
#include "src/imm.h"
#include "src/instruction.h"
#include "src/label.h"
#include "src/m.h"
#include "src/mm.h"
#include "src/modifier.h"
#include "src/moffs.h"
#include "src/r.h"
#include "src/rel.h"
#include "src/sreg.h"
#include "src/st.h"
#include "src/xmm.h"
#include "src/ymm.h"

namespace x64asm {

/** An x64 assembler. */
class Assembler {
	public:
		Function assemble(const Code& code) {
			Function fxn;
			assemble(fxn, code);
			return fxn;
		}

		void assemble(Function& fxn, const Code& code) {
			start(fxn);
			for ( const auto& instr : code )
				assemble(instr);
			finish();
		}

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

		void assemble(const Instruction& instr);

		void bind(Label label) {
			label_defs_[label.val_] = fxn_->size();
		}

		#include "src/assembler.decl"

	private:
		Function* fxn_;
		
		std::unordered_map<uint64_t, size_t> label_defs_;
		std::vector<std::pair<size_t, uint64_t>> label_rels_;

		void pref_group1(uint8_t c) {
			fxn_->emit_byte(c);
		}

		void pref_group2(const M& m) {
			static uint8_t pref[6] {0x26, 0x2e, 0x36, 0x3e, 0x64, 0x65};
			if ( m.contains_seg() )
				fxn_->emit_byte(pref[m.get_seg().val_]);
		}

		void pref_group2(const Hint h) {
			fxn_->emit_byte(h.val_ == 0 ? 0x3e : 0x2e);
		}

		void pref_group3() {
			fxn_->emit_byte(0x66);
		}

		void pref_group4(const M& m) {
			if ( m.get_addr_or() )
				fxn_->emit_byte(0x67);
		}

		void opcode(uint8_t o1) {
			fxn_->emit_byte(o1);
		}

		void opcode(uint8_t o1, const Operand& rcode) {
			const auto delta = rcode.val_ & 0x7;
			opcode(o1+delta);
		}

		void opcode(uint8_t o1, uint8_t o2) {
			fxn_->emit_byte(o1);
			fxn_->emit_byte(o2);
		}

		void opcode(uint8_t o1, uint8_t o2, const Operand& rcode) {
			const auto delta = rcode.val_ & 0x7;
			opcode(o1, o2+delta);
		}

		void opcode(uint8_t o1, uint8_t o2, uint8_t o3) {
			fxn_->emit_byte(o1);
			fxn_->emit_byte(o2);
			fxn_->emit_byte(o3);
		}

		void disp_imm(Imm8 i) {
			fxn_->emit_byte(i.val_);
		}

		void disp_imm(Imm16 i) {
			fxn_->emit_word(i.val_);
		}

		void disp_imm(Imm32 i) {
			fxn_->emit_long(i.val_);
		}

		void disp_imm(Imm64 i) {
			fxn_->emit_quad(i.val_);
		}

		void disp_imm(Label l) {
			label_rels_.push_back(std::make_pair(fxn_->size(), l.val_));
			fxn_->advance_long();
		}

		void disp_imm(const Moffs& m) {
			disp_imm(m.get_offset());
		}

		void disp_imm(Rel8 r) {
			fxn_->emit_byte(r.val_);
		}

		void disp_imm(Rel32 r) {
			fxn_->emit_long(r.val_);
		}

		// Figure 2.4: Intel Manual Vol 2A 2-8
		void rex(const M& rm, const Operand& r, uint8_t val) {
			rex(rm, val | ((r.val_ >> 1) & 0x4));
		}

		// Figure 2.6: Intel Manual Vol 2A 2.9
		void rex(const M& rm, uint8_t val) {
			if ( rm.contains_base() )
				val |= (rm.get_base().val_ >> 3);
			if ( rm.contains_index() )
				val |= ((rm.get_index().val_ >> 2) & 0x2);
			if ( val )
				fxn_->emit_byte(val | 0x40);
		}

		// Figure 2.5: Intel Manual Vol 2A 2-8
		void rex(const Operand& rm, const Operand& r, 
				            uint8_t val) {
			rex(rm, val | ((r.val_ >> 1) & 0x4));
		}

		// Figure 2.7: Intel Manual Vol 2A 2-9
		void rex(const Operand& rm, uint8_t val) {
			if ( val |= (rm.val_ >> 3) )
				fxn_->emit_byte(val | 0x40);	
		}

		void mod_rm_sib(const M& rm, const Operand& r);
		
		void mod_rm_sib(const Operand& rm, const Operand& r) {
			auto mod = 0xc0 | ((r.val_ << 3) & 0x38) | (rm.val_ & 0x7);
			fxn_->emit_byte(mod);
		}

		void resize() {
			if ( fxn_->capacity() - fxn_->size() < 15 ) 
				fxn_->resize(fxn_->capacity()*2);
		}

		// Figure 2-9: Intel Manual Vol 2A 2-14
		// For simplicity, we always emit a three-byte rex prefix. 
		// If we want, we could change the logic here, I believe.
		void vex(uint8_t mmmmm, uint8_t l, uint8_t pp, uint8_t w,
				            const Operand& vvvv) {

			fxn_->emit_byte(0xc4);
			fxn_->emit_byte(mmmmm);
			fxn_->emit_byte((w << 7) | (vvvv.val_ << 3) | (l << 2) | pp); 
		}

		// Figure 2-9: Intel Manual Vol 2A 2-14
		void vex(uint8_t mmmmm, uint8_t l, uint8_t pp, uint8_t w,
				            const Operand& vvvv, const M& rm, 
										const Operand& r) {
			mmmmm |= ((~r.val_ << 4) & 0x80);
			if ( rm.contains_base() )
				mmmmm |= ((~rm.get_base().val_ << 3) & 0x40);
			if ( rm.contains_index() )
				mmmmm |= ((~rm.get_index().val_ << 2) & 0x20);

			vex(mmmmm, l, pp, w, vvvv);
		}

		// Figure 2-9: Intel Manual Vol 2A 2-14
		void vex(uint8_t mmmmm, uint8_t l, uint8_t pp, uint8_t w,
				            const Operand& vvvv, const Operand& rm, 
										const Operand& r) {
			mmmmm |= ((~rm.val_ << 2) & 0x20);
			mmmmm |= ((~r.val_  << 4) & 0x80);

			vex(mmmmm, l, pp, w, vvvv);
		}
};

} // namespace x64asm

#endif
