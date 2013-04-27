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

/** An in-memory assembler. This class can be thought of as an expert
	  user of the Function api. 
*/
class Assembler {
	public:
		/** Resize's a function's internal buffer to guarantee sufficient
			  space for assembling an instruction.
		*/
		void reserve(Function& fxn, const Instruction& instr) {
			fxn.reserve(fxn.size() + 15);
		}

		/** Resize's a function's internal buffer to guarantee sufficient
			  space for assembling a code.
		*/
		void reserve(Function& fxn, const Code& code) {
			fxn.reserve(fxn.size() + 15*code.size());
		}

		/** Convenience method; compiles a code into a newly allocated function. */
		Function assemble(const Code& code) {
			Function fxn;
			reserve(fxn, code);
			assemble(fxn, code);
			return fxn;
		}

		/** Compiles a code into a preallocated function. */
		void assemble(Function& fxn, const Code& code) {
			start(fxn);
			for ( const auto& instr : code )
				assemble(instr);
			finish();
		}

		/** Begin compiling a function. Clears the function's internal buffer.
			  and erases previously stored label definitions.
		*/
		void start(Function& fxn) {
			fxn_ = &fxn;
			fxn_->clear();

			label_defs_.clear();
			label_rels_.clear();
		}	

		/** Finishes compiling a function. Replaces relative placeholders by
			  actual values.
		*/
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

		/** Assembles an instruction. This method will print a hex dump to
				standard error when x64asm is compiled in debug mode.
		*/
		void assemble(const Instruction& instr);

		/** Bind a label definition to the current assembler position. */
		void bind(Label label) {
			label_defs_[label.val_] = fxn_->size();
		}

		// void adc(const Al& arg0, const Imm8& arg1); ...
		#include "src/assembler.decl"

	private:
		/** Pointer to the function being compiled. */
		Function* fxn_;
		
		/** Maps label definitions to code position. */
		std::unordered_map<uint64_t, size_t> label_defs_;
		/** Keeps track of unresolved label references. */
		std::vector<std::pair<size_t, uint64_t>> label_rels_;

		/** Emits an fwait prefix byte. */
		void pref_fwait(uint8_t c) {
			fxn_->emit_byte(c);
		}

		/** Emits a group 1 prefix byte. */
		void pref_group1(uint8_t c) {
			fxn_->emit_byte(c);
		}

		/** Emits a group 2 non-hint prefix byte. */ 
		void pref_group2(const M& m) {
			static uint8_t pref[6] {0x26, 0x2e, 0x36, 0x3e, 0x64, 0x65};
			if ( m.contains_seg() )
				fxn_->emit_byte(pref[m.get_seg().val_]);
		}

		/** Emits a group 2 hint prefix byte. */
		void pref_group2(const Hint h) {
			fxn_->emit_byte(h.val_ == 0 ? 0x3e : 0x2e);
		}

		/** Emits a group 3 prefix byte. */
		void pref_group3() {
			fxn_->emit_byte(0x66);
		}

		/** Emits a group 4 prefix byte. */
		void pref_group4(const M& m) {
			if ( m.get_addr_or() )
				fxn_->emit_byte(0x67);
		}

		/** Emits a one-byte opcode. */
		void opcode(uint8_t o1) {
			fxn_->emit_byte(o1);
		}

		/** Emits a register scaled one-byte opcode. */
		void opcode(uint8_t o1, const Operand& rcode) {
			const auto delta = rcode.val_ & 0x7;
			opcode(o1+delta);
		}

		/** Emits a two-byte opcode. */
		void opcode(uint8_t o1, uint8_t o2) {
			fxn_->emit_byte(o1);
			fxn_->emit_byte(o2);
		}

		/** Emits a register scaled two-byte opcode. */
		void opcode(uint8_t o1, uint8_t o2, const Operand& rcode) {
			const auto delta = rcode.val_ & 0x7;
			opcode(o1, o2+delta);
		}

		/** Emits a three-byte opcode. */
		void opcode(uint8_t o1, uint8_t o2, uint8_t o3) {
			fxn_->emit_byte(o1);
			fxn_->emit_byte(o2);
			fxn_->emit_byte(o3);
		}

		/** Emits a one-byte immediate. */
		void disp_imm(Imm8 i) {
			fxn_->emit_byte(i.val_);
		}

		/** Emits a two-byte immediate. */
		void disp_imm(Imm16 i) {
			fxn_->emit_word(i.val_);
		}

		/** Emits a four-byte immediate. */
		void disp_imm(Imm32 i) {
			fxn_->emit_long(i.val_);
		}

		/** Emits an eight-byte immediate. */
		void disp_imm(Imm64 i) {
			fxn_->emit_quad(i.val_);
		}

		/** Records internal state for a label reference. Saves the current code 
			  position and reserves space for the resolved address.
		*/
		void disp_imm(Label l) {
			label_rels_.push_back(std::make_pair(fxn_->size(), l.val_));
			fxn_->advance_long();
		}

		/** Emits an eight-byte memory offset. */
		void disp_imm(const Moffs& m) {
			disp_imm(m.get_offset());
		}

		/** Emits a one-byte relative address. */
		void disp_imm(Rel8 r) {
			fxn_->emit_byte(r.val_);
		}

		/** Emits a four-byte relative address. */
		void disp_imm(Rel32 r) {
			fxn_->emit_long(r.val_);
		}

		/** Unconditionally emits a rex prefix. */
		void rex(uint8_t val) {
			fxn_->emit_byte(val);
		}

		/** Conditionally emits a rex prefix. 
			  See Figure 2.4: Intel Manual Vol 2A 2-8. 
		*/
		void rex(const M& rm, const Operand& r, uint8_t val) {
			rex(rm, val | ((r.val_ >> 1) & 0x4));
		}

		/** Conditionally emits a rex prefix. 
		    See Figure 2.6: Intel Manual Vol 2A 2.9.
		*/
		void rex(const M& rm, uint8_t val) {
			if ( rm.contains_base() )
				val |= (rm.get_base().val_ >> 3);
			if ( rm.contains_index() )
				val |= ((rm.get_index().val_ >> 2) & 0x2);
			if ( val )
				fxn_->emit_byte(val | 0x40);
		}

		/** Conditonally emits a rex prefix.
		    See Figure 2.5: Intel Manual Vol 2A 2-8.
		*/
		void rex(const Operand& rm, const Operand& r, 
				            uint8_t val) {
			rex(rm, val | ((r.val_ >> 1) & 0x4));
		}

		/** Conditionally emits a rex prefix.
		    See	Figure 2.7: Intel Manual Vol 2A 2-9.
		*/
		void rex(const Operand& rm, uint8_t val) {
			if ( val |= (rm.val_ >> 3) )
				fxn_->emit_byte(val | 0x40);	
		}

		/** Emits a mod/rm sib byte pair. */
		void mod_rm_sib(const M& rm, const Operand& r);
		
		/** Emits a mod/rm sib byte pair. */
		void mod_rm_sib(const Operand& rm, const Operand& r) {
			auto mod = 0xc0 | ((r.val_ << 3) & 0x38) | (rm.val_ & 0x7);
			fxn_->emit_byte(mod);
		}

		/** Emits a 2-byte vex prefix. See Figure 2-9: Intel Manual Vol 2A 2-14. */
		void vex2(uint8_t r_bit, const Operand& vvvv, uint8_t l, uint8_t pp) {
			fxn_->emit_byte(0xc5);
			fxn_->emit_byte(r_bit | (vvvv.val_ << 3) | (l << 2) | pp);
		}

		/** Emits a 3-byte vex prefix. See Figure 2-9: Intel Manual Vol 2A 2-14. */
		void vex3(uint8_t r_bit, uint8_t x_bit, uint8_t b_bit, uint8_t mmmmm,
				      uint8_t w, const Operand& vvvv, uint8_t l, uint8_t pp) {
			fxn_->emit_byte(0xc4);
			fxn_->emit_byte(r_bit | x_bit | b_bit | mmmmm);
			fxn_->emit_byte((w << 7) | (vvvv.val_ << 3) | (l << 2) | pp); 
		}

		// Emits a vex prefix. See Figure 2-9: Intel Manual Vol 2A 2-14. */
		void vex(uint8_t mmmmm, uint8_t l, uint8_t pp, uint8_t w,
             const Operand& vvvv, const M& rm, 
             const Operand& r) {
			uint8_t r_bit = (~r.val_ << 4) & 0x80;
			uint8_t x_bit = rm.contains_base() ? 
											(~rm.get_base().val_ << 3) & 0x40 : 0x40;
			uint8_t b_bit = rm.contains_index() ?
											(~rm.get_index().val_ << 2) & 0x20 : 0x20;

			if ( x_bit == 0x40 && b_bit == 0x20 && mmmmm == 0x01 && w == 0 )
				vex2(r_bit, vvvv, l, pp);
			else
				vex3(r_bit, x_bit, b_bit, mmmmm, w, vvvv, l, pp);
		}

		/** Emits a vex prefix. See Figure 2-9: Intel Manual Vol 2A 2-14. */
		void vex(uint8_t mmmmm, uint8_t l, uint8_t pp, uint8_t w,
				     const Operand& vvvv, const Operand& rm, 
             const Operand& r) {
			uint8_t r_bit = (~r.val_ << 4) & 0x80;
			uint8_t b_bit = (~rm.val_ << 2) & 0x20;

			if ( b_bit == 0x20 && mmmmm == 0x01 && w == 0 )
				vex2(r_bit, vvvv, l, pp);
			else
				vex3(r_bit, 0x40, b_bit, mmmmm, w, vvvv, l, pp);
		}

		/** Emits a vex prefix. See Figure 2-9: Intel Manual Vol 2A 2-14. */
		void vex(uint8_t mmmmm, uint8_t l, uint8_t pp, uint8_t w, 
				     const Operand& vvvv) { 
			if ( mmmmm == 0x01 && w == 0 )
				vex2(0x80, vvvv, l, pp);
			else
				vex3(0x80, 0x40, 0x20, mmmmm, w, vvvv, l, pp);
		}

		/** Prints out the hex encoding of the last instruciton that was assembled.
		    This method is only used when x64asm is compiled in debug mode.
		*/
		void debug(const Instruction& instr, size_t start) const;
};

} // namespace x64asm

#endif
