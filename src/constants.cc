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

#include "src/constants.h"

using namespace std;

namespace x64asm {

const Eflags eflags_cf {
  Constants::eflags_cf()
};
const Eflags eflags_pf {
  Constants::eflags_pf()
};
const Eflags eflags_af {
  Constants::eflags_af()
};
const Eflags eflags_zf {
  Constants::eflags_zf()
};
const Eflags eflags_sf {
  Constants::eflags_sf()
};
const Eflags eflags_tf {
  Constants::eflags_tf()
};
const Eflags eflags_if {
  Constants::eflags_if()
};
const Eflags eflags_df {
  Constants::eflags_df()
};
const Eflags eflags_of {
  Constants::eflags_of()
};
const Eflags eflags_iopl {
  Constants::eflags_iopl()
};
const Eflags eflags_nt {
  Constants::eflags_nt()
};
const Eflags eflags_rf {
  Constants::eflags_rf()
};
const Eflags eflags_vm {
  Constants::eflags_vm()
};
const Eflags eflags_ac {
  Constants::eflags_ac()
};
const Eflags eflags_vif {
  Constants::eflags_vif()
};
const Eflags eflags_vip {
  Constants::eflags_vip()
};
const Eflags eflags_id {
  Constants::eflags_id()
};

const array<Eflags,17> eflags {
  eflags_cf, eflags_pf,   eflags_af,  eflags_zf,
  eflags_sf, eflags_tf,   eflags_if,  eflags_df,
  eflags_of, eflags_iopl, eflags_nt,  eflags_rf,
  eflags_vm, eflags_ac,   eflags_vif, eflags_vip,
  eflags_id
};

const FpuControl fpu_control_im {
  Constants::fpu_control_im()
};
const FpuControl fpu_control_dm {
  Constants::fpu_control_dm()
};
const FpuControl fpu_control_zm {
  Constants::fpu_control_zm()
};
const FpuControl fpu_control_om {
  Constants::fpu_control_om()
};
const FpuControl fpu_control_um {
  Constants::fpu_control_um()
};
const FpuControl fpu_control_pm {
  Constants::fpu_control_pm()
};
const FpuControl fpu_control_pc {
  Constants::fpu_control_pc()
};
const FpuControl fpu_control_rc {
  Constants::fpu_control_rc()
};
const FpuControl fpu_control_x {
  Constants::fpu_control_x()
};

const array<FpuControl,9> fpu_control {
  fpu_control_im, fpu_control_dm, fpu_control_zm, fpu_control_om,
  fpu_control_um, fpu_control_pm, fpu_control_pc, fpu_control_rc,
  fpu_control_x
};

const FpuStatus fpu_status_ie {
  Constants::fpu_status_ie()
};
const FpuStatus fpu_status_de {
  Constants::fpu_status_de()
};
const FpuStatus fpu_status_ze {
  Constants::fpu_status_ze()
};
const FpuStatus fpu_status_oe {
  Constants::fpu_status_oe()
};
const FpuStatus fpu_status_ue {
  Constants::fpu_status_ue()
};
const FpuStatus fpu_status_pe {
  Constants::fpu_status_pe()
};
const FpuStatus fpu_status_sf {
  Constants::fpu_status_sf()
};
const FpuStatus fpu_status_es {
  Constants::fpu_status_es()
};
const FpuStatus fpu_status_c0 {
  Constants::fpu_status_c0()
};
const FpuStatus fpu_status_c1 {
  Constants::fpu_status_c1()
};
const FpuStatus fpu_status_c2 {
  Constants::fpu_status_c2()
};
const FpuStatus fpu_status_top {
  Constants::fpu_status_top()
};
const FpuStatus fpu_status_c3 {
  Constants::fpu_status_c3()
};
const FpuStatus fpu_status_b {
  Constants::fpu_status_b()
};

const array<FpuStatus,14> fpu_status {
  fpu_status_ie, fpu_status_de, fpu_status_ze, fpu_status_oe,
  fpu_status_ue, fpu_status_pe, fpu_status_sf, fpu_status_es,
  fpu_status_c0, fpu_status_c1, fpu_status_c2, fpu_status_top,
  fpu_status_c3, fpu_status_b
};

const FpuTag tag0 {
  Constants::tag0()
};
const FpuTag tag1 {
  Constants::tag1()
};
const FpuTag tag2 {
  Constants::tag2()
};
const FpuTag tag3 {
  Constants::tag3()
};
const FpuTag tag4 {
  Constants::tag4()
};
const FpuTag tag5 {
  Constants::tag5()
};
const FpuTag tag6 {
  Constants::tag6()
};
const FpuTag tag7 {
  Constants::tag7()
};

const array<FpuTag,8> fpu_tags {
  tag0, tag1, tag2, tag3, tag4, tag5, tag6, tag7
};

const Mxcsr mxcsr_ie {
  Constants::mxcsr_ie()
};
const Mxcsr mxcsr_de {
  Constants::mxcsr_de()
};
const Mxcsr mxcsr_ze {
  Constants::mxcsr_ze()
};
const Mxcsr mxcsr_oe {
  Constants::mxcsr_oe()
};
const Mxcsr mxcsr_ue {
  Constants::mxcsr_ue()
};
const Mxcsr mxcsr_pe {
  Constants::mxcsr_pe()
};
const Mxcsr mxcsr_daz {
  Constants::mxcsr_daz()
};
const Mxcsr mxcsr_im {
  Constants::mxcsr_im()
};
const Mxcsr mxcsr_dm {
  Constants::mxcsr_dm()
};
const Mxcsr mxcsr_zm {
  Constants::mxcsr_zm()
};
const Mxcsr mxcsr_om {
  Constants::mxcsr_om()
};
const Mxcsr mxcsr_um {
  Constants::mxcsr_um()
};
const Mxcsr mxcsr_pm {
  Constants::mxcsr_pm()
};
const Mxcsr mxcsr_rc {
  Constants::mxcsr_rc()
};
const Mxcsr mxcsr_fz {
  Constants::mxcsr_fz()
};

const array<Mxcsr,15> mxcsr {
  mxcsr_ie, mxcsr_de, mxcsr_ze,  mxcsr_oe,
  mxcsr_ue, mxcsr_pe, mxcsr_daz, mxcsr_im,
  mxcsr_dm, mxcsr_zm, mxcsr_om,  mxcsr_um,
  mxcsr_pm, mxcsr_rc, mxcsr_fz
};

const FpuData fpu_data {
  Constants::fpu_data()
};
const FpuInstruction fpu_instruction {
  Constants::fpu_instruction()
};
const FpuOpcode fpu_opcode {
  Constants::fpu_opcode()
};
const Rip rip {
  Constants::rip()
};

const Hint taken {
  Constants::taken()
};
const Hint not_taken {
  Constants::not_taken()
};

const Zero zero {
  Constants::zero()
};
const One one {
  Constants::one()
};
const Three three {
  Constants::three()
};

const Mm mm0 {
  Constants::mm0()
};
const Mm mm1 {
  Constants::mm1()
};
const Mm mm2 {
  Constants::mm2()
};
const Mm mm3 {
  Constants::mm3()
};
const Mm mm4 {
  Constants::mm4()
};
const Mm mm5 {
  Constants::mm5()
};
const Mm mm6 {
  Constants::mm6()
};
const Mm mm7 {
  Constants::mm7()
};

const array<Mm,8> mms {
  mm0, mm1, mm2, mm3, mm4, mm5, mm6, mm7
};

const Pref66 pref_66 {
  Constants::pref_66()
};
const PrefRexW pref_rex_w {
  Constants::pref_rex_w()
};
const Far far {
  Constants::far()
};

const Al al {
  Constants::al()
};
const Cl cl {
  Constants::cl()
};
const Rl dl {
  Constants::dl()
};
const Rl bl {
  Constants::bl()
};

const array<Rl,4> rls {
  al, cl, dl, bl
};

const Rh ah {
  Constants::ah()
};
const Rh ch {
  Constants::ch()
};
const Rh dh {
  Constants::dh()
};
const Rh bh {
  Constants::bh()
};

const array<Rh,4> rhs {
  ah, ch, dh, bh
};

const Rb spl {
  Constants::spl()
};
const Rb bpl {
  Constants::bpl()
};
const Rb sil {
  Constants::sil()
};
const Rb dil {
  Constants::dil()
};
const Rb r8b {
  Constants::r8b()
};
const Rb r9b {
  Constants::r9b()
};
const Rb r10b {
  Constants::r10b()
};
const Rb r11b {
  Constants::r11b()
};
const Rb r12b {
  Constants::r12b()
};
const Rb r13b {
  Constants::r13b()
};
const Rb r14b {
  Constants::r14b()
};
const Rb r15b {
  Constants::r15b()
};

const array<Rb,12> rbs {
  spl, bpl, sil, dil, r8b, r9b, r10b, r11b, r12b, r13b, r14b, r15b
};

const Ax ax {
  Constants::ax()
};
const R16 cx {
  Constants::cx()
};
const Dx dx {
  Constants::dx()
};
const R16 bx {
  Constants::bx()
};
const R16 sp {
  Constants::sp()
};
const R16 bp {
  Constants::bp()
};
const R16 si {
  Constants::si()
};
const R16 di {
  Constants::di()
};
const R16 r8w {
  Constants::r8w()
};
const R16 r9w {
  Constants::r9w()
};
const R16 r10w {
  Constants::r10w()
};
const R16 r11w {
  Constants::r11w()
};
const R16 r12w {
  Constants::r12w()
};
const R16 r13w {
  Constants::r13w()
};
const R16 r14w {
  Constants::r14w()
};
const R16 r15w {
  Constants::r15w()
};

const array<R16,16> r16s {
  ax,  cx,  dx,   bx,   sp,   bp,   si,   di,
  r8w, r9w, r10w, r11w, r12w, r13w, r14w, r15w
};

const Eax eax {
  Constants::eax()
};
const R32 ecx {
  Constants::ecx()
};
const R32 edx {
  Constants::edx()
};
const R32 ebx {
  Constants::ebx()
};
const R32 esp {
  Constants::esp()
};
const R32 ebp {
  Constants::ebp()
};
const R32 esi {
  Constants::esi()
};
const R32 edi {
  Constants::edi()
};
const R32 r8d {
  Constants::r8d()
};
const R32 r9d {
  Constants::r9d()
};
const R32 r10d {
  Constants::r10d()
};
const R32 r11d {
  Constants::r11d()
};
const R32 r12d {
  Constants::r12d()
};
const R32 r13d {
  Constants::r13d()
};
const R32 r14d {
  Constants::r14d()
};
const R32 r15d {
  Constants::r15d()
};

const array<R32,16> r32s {
  eax, ecx, edx,  ebx,  esp,  ebp,  esi,  edi,
  r8d, r9d, r10d, r11d, r12d, r13d, r14d, r15d
};

const Rax rax {
  Constants::rax()
};
const R64 rcx {
  Constants::rcx()
};
const R64 rdx {
  Constants::rdx()
};
const R64 rbx {
  Constants::rbx()
};
const R64 rsp {
  Constants::rsp()
};
const R64 rbp {
  Constants::rbp()
};
const R64 rsi {
  Constants::rsi()
};
const R64 rdi {
  Constants::rdi()
};
const R64 r8 {
  Constants::r8()
};
const R64 r9 {
  Constants::r9()
};
const R64 r10 {
  Constants::r10()
};
const R64 r11 {
  Constants::r11()
};
const R64 r12 {
  Constants::r12()
};
const R64 r13 {
  Constants::r13()
};
const R64 r14 {
  Constants::r14()
};
const R64 r15 {
  Constants::r15()
};

const array<R64,16> r64s {
  rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi,
  r8,  r9,  r10, r11, r12, r13, r14, r15
};

const Sreg es {
  Constants::es()
};
const Sreg cs {
  Constants::cs()
};
const Sreg ss {
  Constants::ss()
};
const Sreg ds {
  Constants::ds()
};
const Fs fs {
  Constants::fs()
};
const Gs gs {
  Constants::gs()
};

const array<Sreg,6> sregs {
  es, cs, ss, ds, fs, gs
};

const St0 st0 {
  Constants::st0()
};
const St st1 {
  Constants::st1()
};
const St st2 {
  Constants::st2()
};
const St st3 {
  Constants::st3()
};
const St st4 {
  Constants::st4()
};
const St st5 {
  Constants::st5()
};
const St st6 {
  Constants::st6()
};
const St st7 {
  Constants::st7()
};

const array<St,8> sts {
  st0, st1, st2, st3, st4, st5, st6, st7
};

const Xmm0 xmm0 {
  Constants::xmm0()
};
const Xmm xmm1 {
  Constants::xmm1()
};
const Xmm xmm2 {
  Constants::xmm2()
};
const Xmm xmm3 {
  Constants::xmm3()
};
const Xmm xmm4 {
  Constants::xmm4()
};
const Xmm xmm5 {
  Constants::xmm5()
};
const Xmm xmm6 {
  Constants::xmm6()
};
const Xmm xmm7 {
  Constants::xmm7()
};
const Xmm xmm8 {
  Constants::xmm8()
};
const Xmm xmm9 {
  Constants::xmm9()
};
const Xmm xmm10 {
  Constants::xmm10()
};
const Xmm xmm11 {
  Constants::xmm11()
};
const Xmm xmm12 {
  Constants::xmm12()
};
const Xmm xmm13 {
  Constants::xmm13()
};
const Xmm xmm14 {
  Constants::xmm14()
};
const Xmm xmm15 {
  Constants::xmm15()
};

const array<Xmm,16> xmms {
  xmm0, xmm1, xmm2,  xmm3,  xmm4,  xmm5,  xmm6,  xmm7,
  xmm8, xmm9, xmm10, xmm11, xmm12, xmm13, xmm14, xmm15
};

const Ymm ymm0 {
  Constants::ymm0()
};
const Ymm ymm1 {
  Constants::ymm1()
};
const Ymm ymm2 {
  Constants::ymm2()
};
const Ymm ymm3 {
  Constants::ymm3()
};
const Ymm ymm4 {
  Constants::ymm4()
};
const Ymm ymm5 {
  Constants::ymm5()
};
const Ymm ymm6 {
  Constants::ymm6()
};
const Ymm ymm7 {
  Constants::ymm7()
};
const Ymm ymm8 {
  Constants::ymm8()
};
const Ymm ymm9 {
  Constants::ymm9()
};
const Ymm ymm10 {
  Constants::ymm10()
};
const Ymm ymm11 {
  Constants::ymm11()
};
const Ymm ymm12 {
  Constants::ymm12()
};
const Ymm ymm13 {
  Constants::ymm13()
};
const Ymm ymm14 {
  Constants::ymm14()
};
const Ymm ymm15 {
  Constants::ymm15()
};

const array<Ymm,16> ymms {
  ymm0, ymm1, ymm2,  ymm3,  ymm4,  ymm5,  ymm6,  ymm7,
  ymm8, ymm9, ymm10, ymm11, ymm12, ymm13, ymm14, ymm15
};

} // namespace x64asm
