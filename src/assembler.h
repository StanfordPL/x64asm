#ifndef X64_SRC_ASSEMBLER_H
#define X64_SRC_ASSEMBLER_H

#include <cassert>
#include <iostream>
#include <unordered_map>

#include "src/function.h"
#include "src/code.h"
#include "src/cr.h"
#include "src/dr.h"
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

namespace x64 {

/** An x64 assembler. */
class Assembler {
	public:
		inline Function assemble(const Code& code) {
			Function fxn;
			assemble(fxn, code);
			return fxn;
		}

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
			label_defs_[label.val()] = fxn_->size();
		}

		#include "src/assembler.decl"

		void write_att(std::ostream& os, const Code& c);
		void write_intel(std::ostream& os, const Code& c);
		void write_elf(std::ostream& os, const Code& c);
		void write_hex(std::ostream& os, const Code& c);

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
				fxn_->emit_byte(pref[m.get_seg()->val()]);
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
			const auto delta = rcode.val() & 0x7;
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
			fxn_->emit_byte(i.val());
		}

		inline void disp_imm(Imm16 i) {
			fxn_->emit_word(i.val());
		}

		inline void disp_imm(Imm32 i) {
			fxn_->emit_long(i.val());
		}

		inline void disp_imm(Imm64 i) {
			fxn_->emit_quad(i.val());
		}

		inline void disp_imm(Moffs8 m) {
			fxn_->emit_quad(m.val());
		}

		inline void disp_imm(Moffs16 m) {
			fxn_->emit_quad(m.val());
		}

		inline void disp_imm(Moffs32 m) {
			fxn_->emit_quad(m.val());
		}

		inline void disp_imm(Moffs64 m) {
			fxn_->emit_quad(m.val());
		}

		inline void disp_imm(Rel8 r) {
			fxn_->emit_byte(r.val());
		}

		inline void disp_imm(Rel32 r) {
			fxn_->emit_long(r.val());
		}

		inline void disp_imm(Label l) {
			label_rels_.push_back(std::make_pair(fxn_->size(), l.val()));
			fxn_->advance_long();
		}

		// Figure 2.4: Intel Manual Vol 2A 2-8
		inline void rex(const M& rm, const AtomicOperand& r, uint8_t val) {
			assert(r.check());
			rex(rm, val | ((r.val() >> 1) & 0x4));
		}

		// Figure 2.6: Intel Manual Vol 2A 2.9
		inline void rex(const M& rm, uint8_t val) {
			assert(rm.check());
			if ( rm.contains_base() )
				val |= (rm.get_base()->val() >> 3);
			if ( rm.contains_index() )
				val |= ((rm.get_index()->val() >> 2) & 0x2);
			if ( val )
				fxn_->emit_byte(val | 0x40);
		}

		// Figure 2.5: Intel Manual Vol 2A 2-8
		inline void rex(const AtomicOperand& rm, const AtomicOperand& r, 
				            uint8_t val) {
			assert(r.check());
			rex(rm, val | ((r.val() >> 1) & 0x4));
		}

		// Figure 2.7: Intel Manual Vol 2A 2-9
		inline void rex(const AtomicOperand& rm, uint8_t val) {
			assert(rm.check());
			if ( val |= (rm.val() >> 3) )
				fxn_->emit_byte(val | 0x40);	
		}

		void mod_rm_sib(const M& rm, const AtomicOperand& r);
		
		inline void mod_rm_sib(const AtomicOperand& rm, const AtomicOperand& r) {
			auto mod = 0xc0 | ((r.val() << 3) & 0x38) | (rm.val() & 0x7);
			fxn_->emit_byte(mod);
		}

		inline void resize() {
			if ( fxn_->capacity() - fxn_->size() < 15 ) 
				fxn_->resize(fxn_->capacity()*2);
		}

		void write_txt(std::ostream& os, const Code& c, bool att);
};

} // namespace x64

#endif
