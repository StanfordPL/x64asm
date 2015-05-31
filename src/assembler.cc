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

#include "src/assembler.h"

#include <cassert>

#include "src/constants.h"

using namespace std;

namespace x64asm {

// void Assembler::adcb(Al arg0, Imm8 arg1) { } ...
#include "src/assembler.defn"

void Assembler::assemble(const Instruction& instr) {
  switch (instr.get_opcode()) {
  case LABEL_DEFN:
    bind(instr.get_operand<Label>(0));
    break;
    // 4000-way switch
#include "src/assembler.switch"

  default:
    assert(false);
  }
}

template <typename T>
void Assembler::mod_rm_sib(const M<T>& rm, const Operand& r) {
  // Every path we take needs these bits for the mod/rm byte
  const auto rrr = (r.val_ << 3) & 0x38;

  // First special case check for RIP+disp32
  if (rm.rip_offset()) {
    const auto mod_byte = 0x00 | rrr | 0x5;
    fxn_->emit_byte(mod_byte);
    disp_imm(rm.get_disp());
    return;
  }

  // Second special case check for no base register
  if (!rm.contains_base()) {
    const auto mod_byte = 0x00 | rrr | 0x4;
    const auto sib_byte = rm.contains_index() ?
                          (((int)rm.get_scale() << 6) & 0xc0) | ((rm.get_index().val_ << 3) & 0x38) | 0x5 :
                          0x00 | 0x20 | 0x5;

    fxn_->emit_byte(mod_byte);
    fxn_->emit_byte(sib_byte);
    disp_imm(rm.get_disp());

    return;
  }

  // Every path we take now requires the non-null base value.
  const auto bbb = rm.get_base().val_ & 0x7;

  // This logic determines what the value of the mod bits will be.
  // It also controls how many immediate bytes we emit later.
  const auto disp = (int32_t)rm.get_disp().val_;
  size_t mod = 0x40;
  if (disp < -128 || disp >= 128) {
    mod = 0x80;
  } else if (disp == 0 && bbb != 0x5) {
    mod = 0x00;
  }

  // Is index non-null?
  if (rm.contains_index()) {
    const auto mod_byte = mod | rrr | 0x4;
    const auto sib_byte = (((int)rm.get_scale() << 6) & 0xc0) |
                          ((rm.get_index().val_ << 3) & 0x38) | bbb;

    fxn_->emit_byte(mod_byte);
    fxn_->emit_byte(sib_byte);
  }
  // Is base sitting in the eip/rip+disp32 row?
  else if (bbb == 0x4) {
    const auto mod_byte = mod | rrr | 0x4;
    const auto sib_byte = (((int)rm.get_scale() << 6) & 0xc0) | 0x20 | bbb;

    fxn_->emit_byte(mod_byte);
    fxn_->emit_byte(sib_byte);
  }
  // No sib byte
  else {
    const auto mod_byte = mod | rrr | bbb;
    fxn_->emit_byte(mod_byte);
  }

  // This logic parallels the logic for the mod bit
  if (mod == 0x40) {
    disp_imm(Imm8(disp));
  } else if (mod == 0x80) {
    disp_imm(Imm32(disp));
  }
}

void Assembler::debug(const Instruction& instr, size_t idx) const {
  const auto fmt = cerr.flags();

  cerr << instr << endl;
  cerr << "  ";

  for (; idx < fxn_->size(); ++idx) {
    cerr << hex << noshowbase << setw(2) << setfill('0');
    cerr << (int)fxn_->buffer_[idx] << " ";
  }

  cerr << endl;

  cerr.flags(fmt);
}

} // namespace x64asm
