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

#include <cassert>
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
		inline Function assemble(const Code& code) {
			Function fxn;
			assemble(fxn, code);
			return fxn;
		}

		void write_hex(std::ostream& os, const Code& c);

		inline void assemble(Function& fxn, const Code& code) {
			start(fxn);
			for ( const auto& instr : code )
				assemble(instr);
			finish();
		}

		inline void start(Function& fxn) {
			fxn_ = &fxn;
			fxn_->clear();

			label_defs_.clear();
			label_rels_.clear();
		}	

		inline void finish() { 
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

		inline void bind(Label label) {
			label_defs_[label.val_] = fxn_->size();
		}

		#include "src/assembler.decl"

	private:
		Function* fxn_;
		
		std::unordered_map<uint64_t, size_t> label_defs_;
		std::vector<std::pair<size_t, uint64_t>> label_rels_;

		inline void pref_group1(uint8_t c) {
			fxn_->emit_byte(c);
		}

		inline void pref_group2(const M& m) {
			static uint8_t pref[6] {0x26, 0x2e, 0x36, 0x3e, 0x64, 0x65};
			if ( m.contains_seg() )
				fxn_->emit_byte(pref[m.get_seg()->val_]);
		}

		inline void pref_group2(const Hint h) {
			fxn_->emit_byte(h == Hint::TAKEN ? 0x3e : 0x2e);
		}

		inline void pref_group3() {
			fxn_->emit_byte(0x66);
		}

		inline void pref_group4(const M& m) {
			if ( m.get_addr_or() )
				fxn_->emit_byte(0x67);
		}

		inline void opcode(uint8_t o1) {
			fxn_->emit_byte(o1);
		}

		inline void opcode(uint8_t o1, AtomicOperand& rcode) {
			const auto delta = rcode.val_ & 0x7;
			opcode(o1+delta);
		}

		inline void opcode(uint8_t o1, uint8_t o2) {
			fxn_->emit_byte(o1);
			fxn_->emit_byte(o2);
		}

		inline void opcode(uint8_t o1, uint8_t o2, AtomicOperand& rcode) {
			const auto delta = rcode.val_ & 0x7;
			opcode(o1, o2+delta);
		}

		inline void opcode(uint8_t o1, uint8_t o2, uint8_t o3) {
			fxn_->emit_byte(o1);
			fxn_->emit_byte(o2);
			fxn_->emit_byte(o3);
		}

		template <typename T>
		inline typename std::enable_if<!std::is_same<T,Label>::value, void>::type 
				disp_imm(T t) {
			fxn_->emit_byte(t.val_);
		}

		inline void disp_imm(Label l) {
			label_rels_.push_back(std::make_pair(fxn_->size(), l.val_));
			fxn_->advance_long();
		}

		// Figure 2.4: Intel Manual Vol 2A 2-8
		inline void rex(const M& rm, const AtomicOperand& r, uint8_t val) {
			assert(r.check());
			rex(rm, val | ((r.val_ >> 1) & 0x4));
		}

		// Figure 2.6: Intel Manual Vol 2A 2.9
		inline void rex(const M& rm, uint8_t val) {
			assert(rm.check());

			if ( rm.contains_base() )
				val |= (rm.get_base()->val_ >> 3);
			if ( rm.contains_index() )
				val |= ((rm.get_index()->val_ >> 2) & 0x2);
			if ( val )
				fxn_->emit_byte(val | 0x40);
		}

		// Figure 2.5: Intel Manual Vol 2A 2-8
		inline void rex(const AtomicOperand& rm, const AtomicOperand& r, 
				            uint8_t val) {
			assert(r.check());
			rex(rm, val | ((r.val_ >> 1) & 0x4));
		}

		// Figure 2.7: Intel Manual Vol 2A 2-9
		inline void rex(const AtomicOperand& rm, uint8_t val) {
			assert(rm.check());
			if ( val |= (rm.val_ >> 3) )
				fxn_->emit_byte(val | 0x40);	
		}

		void mod_rm_sib(const M& rm, const AtomicOperand& r);
		
		inline void mod_rm_sib(const AtomicOperand& rm, const AtomicOperand& r) {
			auto mod = 0xc0 | ((r.val_ << 3) & 0x38) | (rm.val_ & 0x7);
			fxn_->emit_byte(mod);
		}

		inline void resize() {
			if ( fxn_->capacity() - fxn_->size() < 15 ) 
				fxn_->resize(fxn_->capacity()*2);
		}

		// Figure 2-9: Intel Manual Vol 2A 2-14
		// For simplicity, we always emit a three-byte rex prefix. 
		// If we want, we could change the logic here, I believe.
		inline void vex(uint8_t mmmmm, uint8_t l, uint8_t pp, uint8_t w,
				            const AtomicOperand& vvvv) {
			assert(vvvv.check());

			fxn_->emit_byte(0xc4);
			fxn_->emit_byte(mmmmm);
			fxn_->emit_byte((w << 7) | (vvvv.val_ << 3) | (l << 2) | pp); 
		}

		// Figure 2-9: Intel Manual Vol 2A 2-14
		inline void vex(uint8_t mmmmm, uint8_t l, uint8_t pp, uint8_t w,
				            const AtomicOperand& vvvv, const M& rm, 
										const AtomicOperand& r) {
			assert(rm.check());
			assert(r.check());	

			mmmmm |= ((~r.val_ << 4) & 0x80);
			if ( rm.contains_base() )
				mmmmm |= ((~rm.get_base()->val_ << 3) & 0x40);
			if ( rm.contains_index() )
				mmmmm |= ((~rm.get_index()->val_ << 2) & 0x20);

			vex(mmmmm, l, pp, w, vvvv);
		}

		// Figure 2-9: Intel Manual Vol 2A 2-14
		inline void vex(uint8_t mmmmm, uint8_t l, uint8_t pp, uint8_t w,
				            const AtomicOperand& vvvv, const AtomicOperand& rm, 
										const AtomicOperand& r) {
			assert(rm.check());
			assert(r.check());

			mmmmm |= ((~rm.val_ << 2) & 0x20);
			mmmmm |= ((~r.val_  << 4) & 0x80);

			vex(mmmmm, l, pp, w, vvvv);
		}
};

} // namespace x64asm

#endif
