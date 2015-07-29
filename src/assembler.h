/*
Copyright 2013-2015 Stanford University

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
    fxn.reserve(fxn.size() + 15 * code.size());
  }

  /** Convenience method; compiles a code into a newly allocated function. */
  std::pair<bool,Function> assemble(const Code& code) {
    Function fxn;
    reserve(fxn, code);
    bool ok = assemble(fxn, code);
    return std::pair<bool,Function>(ok, fxn);
  }

  /** Compiles a code into a preallocated function. */
  bool assemble(Function& fxn, const Code& code) {
    start(fxn);
    for (const auto & instr : code) {
      assemble(instr);
    }
    return finish();
  }

  /** Returns the number of bytes in the hex encoding of this instruction */
  size_t hex_size(const Instruction& instr) {
    static Function f;

    const auto backup = fxn_;
    start(f);
    assemble(instr);
    fxn_ = backup;

    return f.size();
  }

  /** Begin compiling a function. Clears the function's internal buffer.
      and erases previously stored label definitions.
  */
  void start(Function& fxn) {
    fxn_ = &fxn;
    fxn_->clear();
  }


  /** Finishes compiling a function. Replaces relative placeholders by
      actual values, and deletes references after doing so.
  */
  bool finish() {
    std::vector<std::pair<size_t, uint64_t>> unresolved;

    for (const auto & l : fxn_->label32_rels_) {
      const auto pos = l.first;
      const auto itr = fxn_->label_defs_.find(l.second);

      if (itr == fxn_->label_defs_.end()) {
        unresolved.push_back(l);
      } else {
        fxn_->emit_long(itr->second-pos-4, pos);
      }
    }

    fxn_->label32_rels_ = unresolved;
    unresolved.clear();

    for (const auto & l : fxn_->label8_rels_) {
      const auto pos = l.first;
      const auto itr = fxn_->label_defs_.find(l.second);

      if (itr == fxn_->label_defs_.end()) {
        unresolved.push_back(l);
      } else {
        const auto diff = itr->second-pos-1;
        if(diff > 0x7f && diff < 0xffffffffffffff80)
          return false;
        fxn_->emit_byte(diff & 0xff, pos);
      }
    }

    fxn_->label8_rels_ = unresolved;
    return true;
  }

  /** Assembles an instruction. This method will print a hex dump to
      standard error when x64asm is compiled in debug mode.
  */
  void assemble(const Instruction& instr);

  /** Bind a label definition to the current assembler position. */
  void bind(Label label) {
    fxn_->label_defs_[label.val_] = fxn_->size();
  }

  // void adc(const Al& arg0, const Imm8& arg1); ...
  #include "src/assembler.decl"

private:
  /** Pointer to the function being compiled. */
  Function* fxn_;

  /** REX prefix constant */
  constexpr uint8_t rex() {
    return 0x40;
  }
  /** REX.W prefix constant */
  constexpr uint8_t rex_w() {
    return 0x48;
  }
  /** REX.R prefix constant */
  constexpr uint8_t rex_r() {
    return 0x44;
  }
  /** REX.X prefix constant */
  constexpr uint8_t rex_x() {
    return 0x42;
  }
  /** REX.B prefix constant */
  constexpr uint8_t rex_b() {
    return 0x41;
  }

  /** Does this operand require a REX prefix? */
  constexpr bool requires_rex_byte(const Operand& o) {
    return o.type() == Type::R_8 && o.val_ > 3;
  }
  /** Does this operand require a REX.[R,X,B] prefix? */
  constexpr bool requires_rex_bit(const Operand& o) {
    return o.val_ > 7;
  }

  /** Emits an fwait prefix byte. */
  void pref_fwait(uint8_t c) {
    fxn_->emit_byte(c);
  }

  /** Emits a group 1 prefix byte. */
  void pref_group1(uint8_t c) {
    fxn_->emit_byte(c);
  }

  /** Emits a group 2 non-hint prefix byte. */
  template <typename T>
  void pref_group2(const M<T>& m) {
    static uint8_t pref[6] {0x26, 0x2e, 0x36, 0x3e, 0x64, 0x65};
    if (m.contains_seg()) {
      fxn_->emit_byte(pref[m.get_seg().val_]);
    }
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
  template <typename T>
  void pref_group4(const M<T>& m) {
    if (m.addr_or()) {
      fxn_->emit_byte(0x67);
    }
  }

  /** Emits a one-byte opcode. */
  void opcode(uint8_t o1) {
    fxn_->emit_byte(o1);
  }

  /** Emits a register scaled one-byte opcode. */
  void opcode(uint8_t o1, const Operand& rcode) {
    const auto delta = rcode.val_ & 0x7;
    opcode(o1 + delta);
  }

  /** Emits a two-byte opcode. */
  void opcode(uint8_t o1, uint8_t o2) {
    fxn_->emit_byte(o1);
    fxn_->emit_byte(o2);
  }

  /** Emits a register scaled two-byte opcode. */
  void opcode(uint8_t o1, uint8_t o2, const Operand& rcode) {
    const auto delta = rcode.val_ & 0x7;
    opcode(o1, o2 + delta);
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

  /** Emits a pair of immediates. */
  void disp_imm(Imm8 i1, Imm16 i2) {
    disp_imm(i2);
    disp_imm(i1);
  }

  /** Records internal state for a label reference. Saves the current code
      position and reserves space for the resolved address by emitting zero
      bytes.
  */
  void disp_label8(Label l) {
    fxn_->label8_rels_.push_back(std::make_pair(fxn_->size(), l.val_));
    fxn_->emit_byte(0);
  }

  void disp_label32(Label l) {
    fxn_->label32_rels_.push_back(std::make_pair(fxn_->size(), l.val_));
    fxn_->emit_long(0);
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

  /** Emits an xmm register encoded as an immediats. */
  void disp_imm(Xmm x) {
    fxn_->emit_byte(x.val_ << 4);
  }

  /** Emits a ymm register encoded as an immediats. */
  void disp_imm(Ymm y) {
    fxn_->emit_byte(y.val_ << 4);
  }

  /** Conditionally emits a rex prefix; zero values are not printed. */
  void rex(uint8_t byte) {
    if (byte) {
      fxn_->emit_byte(byte);
    }
  }

  /** Emits a rex prefix for rm/r memory/register instructions */
  template <typename T>
  void rex(const M<T>& rm, const Operand& r, uint8_t byte) {
    byte |= requires_rex_byte(r) ? rex() : 0x00;
    byte |= requires_rex_bit(r) ? rex_r() : 0x00;
    rex(rm, byte);
  }

  /** Emits a rex prefix for rm memory instructions */
  template <typename T>
  void rex(const M<T>& rm, uint8_t byte) {
    byte |= (rm.contains_base() && requires_rex_bit(rm.get_base())) ?
      rex_b() : 0x00;
    byte |= (rm.contains_index() && requires_rex_bit(rm.get_index())) ?
      rex_x() : 0x00;
    rex(byte);
  }

  /** Emits a rex prefix for rm/r register/register instructions */
  void rex(const Operand& rm, const Operand& r, uint8_t byte) {
    byte |= requires_rex_byte(r) ? rex() : 0x00;
    byte |= requires_rex_bit(r) ? rex_r() : 0x00;
    rex(rm, byte);
  }

  /** Emits a rex prefix for rm register instructions */
  void rex(const Operand& rm, uint8_t byte) {
    byte |= requires_rex_byte(rm) ? rex() : 0x00;
    byte |= requires_rex_bit(rm) ? rex_b() : 0x00;
    rex(byte);
  }

  /** Emits a mod/rm sib byte pair. */
  template <typename T>
  void mod_rm_sib(const M<T>& rm, const Operand& r);

  /** Emits a mod/rm sib byte pair. */
  void mod_rm_sib(const Operand& rm, const Operand& r) {
    auto mod = 0xc0 | ((r.val_ << 3) & 0x38) | (rm.val_ & 0x7);
    fxn_->emit_byte(mod);
  }

  /** Emits a 2-byte vex prefix. See Figure 2-9: Intel Manual Vol 2A 2-14. */
  void vex2(uint8_t r_bit, const Operand& vvvv, uint8_t l, uint8_t pp) {
    fxn_->emit_byte(0xc5);
    fxn_->emit_byte(r_bit | ((~vvvv.val_ << 3) & 0x78) | (l << 2) | pp);
  }

  /** Emits a 3-byte vex prefix. See Figure 2-9: Intel Manual Vol 2A 2-14. */
  void vex3(uint8_t r_bit, uint8_t x_bit, uint8_t b_bit, uint8_t mmmmm,
            uint8_t w, const Operand& vvvv, uint8_t l, uint8_t pp) {
    fxn_->emit_byte(0xc4);
    fxn_->emit_byte(r_bit | x_bit | b_bit | mmmmm);
    fxn_->emit_byte((w << 7) | ((~vvvv.val_ << 3) & 0x78) | (l << 2) | pp);
  }

  // Emits a vex prefix. See Figure 2-9: Intel Manual Vol 2A 2-14. */
  template <typename T>
  void vex(uint8_t mmmmm, uint8_t l, uint8_t pp, uint8_t w,
           const Operand& vvvv, const M<T>& rm,
           const Operand& r) {
    uint8_t r_bit = (~r.val_ << 4) & 0x80;
    uint8_t x_bit = rm.contains_index() ?
                    (~rm.get_index().val_ << 3) & 0x40 : 0x40;
    uint8_t b_bit = rm.contains_base() ?
                    (~rm.get_base().val_ << 2) & 0x20 : 0x20;

    if (x_bit == 0x40 && b_bit == 0x20 && mmmmm == 0x01 && w == 0) {
      vex2(r_bit, vvvv, l, pp);
    } else {
      vex3(r_bit, x_bit, b_bit, mmmmm, w, vvvv, l, pp);
    }
  }

  /** Emits a vex prefix. See Figure 2-9: Intel Manual Vol 2A 2-14. */
  void vex(uint8_t mmmmm, uint8_t l, uint8_t pp, uint8_t w,
           const Operand& vvvv, const Operand& rm,
           const Operand& r) {
    uint8_t r_bit = (~r.val_ << 4) & 0x80;
    uint8_t b_bit = (~rm.val_ << 2) & 0x20;

    if (b_bit == 0x20 && mmmmm == 0x01 && w == 0) {
      vex2(r_bit, vvvv, l, pp);
    } else {
      vex3(r_bit, 0x40, b_bit, mmmmm, w, vvvv, l, pp);
    }
  }

  /** Emits a vex prefix. See Figure 2-9: Intel Manual Vol 2A 2-14. */
  void vex(uint8_t mmmmm, uint8_t l, uint8_t pp, uint8_t w,
           const Operand& vvvv) {
    if (mmmmm == 0x01 && w == 0) {
      vex2(0x80, vvvv, l, pp);
    } else {
      vex3(0x80, 0x40, 0x20, mmmmm, w, vvvv, l, pp);
    }
  }

  /** Prints out the hex encoding of the last instruciton that was assembled.
      This method is only used when x64asm is compiled in debug mode.
  */
  void debug(const Instruction& instr, size_t start) const;
};

} // namespace x64asm

#endif
