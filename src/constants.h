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

#ifndef X64ASM_SRC_CONSTANTS_H
#define X64ASM_SRC_CONSTANTS_H

#include "src/env_bits.h"
#include "src/env_reg.h"
#include "src/hint.h"
#include "src/imm.h"
#include "src/mm.h"
#include "src/modifier.h"
#include "src/r.h"
#include "src/sreg.h"
#include "src/st.h"
#include "src/xmm.h"
#include "src/ymm.h"

namespace x64asm {

/** Predefined assembler constants. Direct access to the object constructors is
    disallowed in general. */
class Constants {
  public:
    static constexpr Eflags eflags_cf()   {
      return Eflags {0, 1};
    }
    static constexpr Eflags eflags_pf()   {
      return Eflags {2, 1};
    }
    static constexpr Eflags eflags_af()   {
      return Eflags {4, 1};
    }
    static constexpr Eflags eflags_zf()   {
      return Eflags {6, 1};
    }
    static constexpr Eflags eflags_sf()   {
      return Eflags {7, 1};
    }
    static constexpr Eflags eflags_tf()   {
      return Eflags {8, 1};
    }
    static constexpr Eflags eflags_if()   {
      return Eflags {9, 1};
    }
    static constexpr Eflags eflags_df()   {
      return Eflags {10, 1};
    }
    static constexpr Eflags eflags_of()   {
      return Eflags {11, 1};
    }
    static constexpr Eflags eflags_iopl() {
      return Eflags {13, 1};
    }
    static constexpr Eflags eflags_nt()   {
      return Eflags {14, 1};
    }
    static constexpr Eflags eflags_rf()   {
      return Eflags {16, 1};
    }
    static constexpr Eflags eflags_vm()   {
      return Eflags {17, 1};
    }
    static constexpr Eflags eflags_ac()   {
      return Eflags {18, 1};
    }
    static constexpr Eflags eflags_vif()  {
      return Eflags {19, 1};
    }
    static constexpr Eflags eflags_vip()  {
      return Eflags {20, 1};
    }
    static constexpr Eflags eflags_id()   {
      return Eflags {21, 1};
    }

    static constexpr FpuControl fpu_control_im() {
      return FpuControl {0, 1};
    }
    static constexpr FpuControl fpu_control_dm() {
      return FpuControl {1, 1};
    }
    static constexpr FpuControl fpu_control_zm() {
      return FpuControl {2, 1};
    }
    static constexpr FpuControl fpu_control_om() {
      return FpuControl {3, 1};
    }
    static constexpr FpuControl fpu_control_um() {
      return FpuControl {4, 1};
    }
    static constexpr FpuControl fpu_control_pm() {
      return FpuControl {5, 1};
    }
    static constexpr FpuControl fpu_control_pc() {
      return FpuControl {9, 2};
    }
    static constexpr FpuControl fpu_control_rc() {
      return FpuControl {11, 2};
    }
    static constexpr FpuControl fpu_control_x()  {
      return FpuControl {12, 1};
    }

    static constexpr FpuStatus fpu_status_ie()  {
      return FpuStatus {0, 1};
    }
    static constexpr FpuStatus fpu_status_de()  {
      return FpuStatus {1, 1};
    }
    static constexpr FpuStatus fpu_status_ze()  {
      return FpuStatus {2, 1};
    }
    static constexpr FpuStatus fpu_status_oe()  {
      return FpuStatus {3, 1};
    }
    static constexpr FpuStatus fpu_status_ue()  {
      return FpuStatus {4, 1};
    }
    static constexpr FpuStatus fpu_status_pe()  {
      return FpuStatus {5, 1};
    }
    static constexpr FpuStatus fpu_status_sf()  {
      return FpuStatus {6, 1};
    }
    static constexpr FpuStatus fpu_status_es()  {
      return FpuStatus {7, 1};
    }
    static constexpr FpuStatus fpu_status_c0()  {
      return FpuStatus {8, 1};
    }
    static constexpr FpuStatus fpu_status_c1()  {
      return FpuStatus {9, 1};
    }
    static constexpr FpuStatus fpu_status_c2()  {
      return FpuStatus {10, 1};
    }
    static constexpr FpuStatus fpu_status_top() {
      return FpuStatus {13, 3};
    }
    static constexpr FpuStatus fpu_status_c3()  {
      return FpuStatus {14, 1};
    }
    static constexpr FpuStatus fpu_status_b()   {
      return FpuStatus {15, 1};
    }

    static constexpr FpuTag tag0() {
      return FpuTag {1, 2};
    }
    static constexpr FpuTag tag1() {
      return FpuTag {3, 2};
    }
    static constexpr FpuTag tag2() {
      return FpuTag {5, 2};
    }
    static constexpr FpuTag tag3() {
      return FpuTag {7, 2};
    }
    static constexpr FpuTag tag4() {
      return FpuTag {9, 2};
    }
    static constexpr FpuTag tag5() {
      return FpuTag {11, 2};
    }
    static constexpr FpuTag tag6() {
      return FpuTag {13, 2};
    }
    static constexpr FpuTag tag7() {
      return FpuTag {15, 2};
    }

    static constexpr Mxcsr mxcsr_ie()  {
      return Mxcsr {0, 1};
    }
    static constexpr Mxcsr mxcsr_de()  {
      return Mxcsr {1, 1};
    }
    static constexpr Mxcsr mxcsr_ze()  {
      return Mxcsr {2, 1};
    }
    static constexpr Mxcsr mxcsr_oe()  {
      return Mxcsr {3, 1};
    }
    static constexpr Mxcsr mxcsr_ue()  {
      return Mxcsr {4, 1};
    }
    static constexpr Mxcsr mxcsr_pe()  {
      return Mxcsr {5, 1};
    }
    static constexpr Mxcsr mxcsr_daz() {
      return Mxcsr {6, 1};
    }
    static constexpr Mxcsr mxcsr_im()  {
      return Mxcsr {7, 1};
    }
    static constexpr Mxcsr mxcsr_dm()  {
      return Mxcsr {8, 1};
    }
    static constexpr Mxcsr mxcsr_zm()  {
      return Mxcsr {9, 1};
    }
    static constexpr Mxcsr mxcsr_om()  {
      return Mxcsr {10, 1};
    }
    static constexpr Mxcsr mxcsr_um()  {
      return Mxcsr {11, 1};
    }
    static constexpr Mxcsr mxcsr_pm()  {
      return Mxcsr {12, 1};
    }
    static constexpr Mxcsr mxcsr_rc()  {
      return Mxcsr {14, 2};
    }
    static constexpr Mxcsr mxcsr_fz()  {
      return Mxcsr {15, 1};
    }

    static constexpr Rip rip() {
      return Rip {};
    }
    static constexpr FpuData fpu_data() {
      return FpuData {};
    }
    static constexpr FpuInstruction fpu_instruction() {
      return FpuInstruction {};
    }
    static constexpr FpuOpcode fpu_opcode() {
      return FpuOpcode {};
    }

    static constexpr Hint taken() {
      return Hint {0};
    }
    static constexpr Hint not_taken() {
      return Hint {1};
    }

    static constexpr Zero zero() {
      return Zero {};
    }
    static constexpr One one() {
      return One {};
    }
    static constexpr Three three() {
      return Three {};
    }

    static constexpr Mm mm0() {
      return Mm {0};
    }
    static constexpr Mm mm1() {
      return Mm {1};
    }
    static constexpr Mm mm2() {
      return Mm {2};
    }
    static constexpr Mm mm3() {
      return Mm {3};
    }
    static constexpr Mm mm4() {
      return Mm {4};
    }
    static constexpr Mm mm5() {
      return Mm {5};
    }
    static constexpr Mm mm6() {
      return Mm {6};
    }
    static constexpr Mm mm7() {
      return Mm {7};
    }

    static constexpr Pref66 pref_66()      {
      return Pref66 {};
    }
    static constexpr PrefRexW pref_rex_w() {
      return PrefRexW {};
    }
    static constexpr Far far()             {
      return Far {};
    }

    static constexpr Al al() {
      return Al {};
    }
    static constexpr Cl cl() {
      return Cl {};
    }
    static constexpr Rl dl() {
      return Rl {2};
    }
    static constexpr Rl bl() {
      return Rl {3};
    }

    static constexpr Rh ah() {
      return Rh {4};
    }
    static constexpr Rh ch() {
      return Rh {5};
    }
    static constexpr Rh dh() {
      return Rh {6};
    }
    static constexpr Rh bh() {
      return Rh {7};
    }

    static constexpr Rb spl()  {
      return Rb {4};
    }
    static constexpr Rb bpl()  {
      return Rb {5};
    }
    static constexpr Rb sil()  {
      return Rb {6};
    }
    static constexpr Rb dil()  {
      return Rb {7};
    }
    static constexpr Rb r8b()  {
      return Rb {8};
    }
    static constexpr Rb r9b()  {
      return Rb {9};
    }
    static constexpr Rb r10b() {
      return Rb {10};
    }
    static constexpr Rb r11b() {
      return Rb {11};
    }
    static constexpr Rb r12b() {
      return Rb {12};
    }
    static constexpr Rb r13b() {
      return Rb {13};
    }
    static constexpr Rb r14b() {
      return Rb {14};
    }
    static constexpr Rb r15b() {
      return Rb {15};
    }

    static constexpr Ax ax()    {
      return Ax {};
    }
    static constexpr R16 cx()   {
      return R16 {1};
    }
    static constexpr Dx dx()    {
      return Dx {};
    }
    static constexpr R16 bx()   {
      return R16 {3};
    }
    static constexpr R16 sp()   {
      return R16 {4};
    }
    static constexpr R16 bp()   {
      return R16 {5};
    }
    static constexpr R16 si()   {
      return R16 {6};
    }
    static constexpr R16 di()   {
      return R16 {7};
    }
    static constexpr R16 r8w()  {
      return R16 {8};
    }
    static constexpr R16 r9w()  {
      return R16 {9};
    }
    static constexpr R16 r10w() {
      return R16 {10};
    }
    static constexpr R16 r11w() {
      return R16 {11};
    }
    static constexpr R16 r12w() {
      return R16 {12};
    }
    static constexpr R16 r13w() {
      return R16 {13};
    }
    static constexpr R16 r14w() {
      return R16 {14};
    }
    static constexpr R16 r15w() {
      return R16 {15};
    }

    static constexpr Eax eax()  {
      return Eax {};
    }
    static constexpr R32 ecx()  {
      return R32 {1};
    }
    static constexpr R32 edx()  {
      return R32 {2};
    }
    static constexpr R32 ebx()  {
      return R32 {3};
    }
    static constexpr R32 esp()  {
      return R32 {4};
    }
    static constexpr R32 ebp()  {
      return R32 {5};
    }
    static constexpr R32 esi()  {
      return R32 {6};
    }
    static constexpr R32 edi()  {
      return R32 {7};
    }
    static constexpr R32 r8d()  {
      return R32 {8};
    }
    static constexpr R32 r9d()  {
      return R32 {9};
    }
    static constexpr R32 r10d() {
      return R32 {10};
    }
    static constexpr R32 r11d() {
      return R32 {11};
    }
    static constexpr R32 r12d() {
      return R32 {12};
    }
    static constexpr R32 r13d() {
      return R32 {13};
    }
    static constexpr R32 r14d() {
      return R32 {14};
    }
    static constexpr R32 r15d() {
      return R32 {15};
    }

    static constexpr Rax rax() {
      return Rax {};
    }
    static constexpr R64 rcx() {
      return R64 {1};
    }
    static constexpr R64 rdx() {
      return R64 {2};
    }
    static constexpr R64 rbx() {
      return R64 {3};
    }
    static constexpr R64 rsp() {
      return R64 {4};
    }
    static constexpr R64 rbp() {
      return R64 {5};
    }
    static constexpr R64 rsi() {
      return R64 {6};
    }
    static constexpr R64 rdi() {
      return R64 {7};
    }
    static constexpr R64 r8()  {
      return R64 {8};
    }
    static constexpr R64 r9()  {
      return R64 {9};
    }
    static constexpr R64 r10() {
      return R64 {10};
    }
    static constexpr R64 r11() {
      return R64 {11};
    }
    static constexpr R64 r12() {
      return R64 {12};
    }
    static constexpr R64 r13() {
      return R64 {13};
    }
    static constexpr R64 r14() {
      return R64 {14};
    }
    static constexpr R64 r15() {
      return R64 {15};
    }

    static constexpr Sreg es() {
      return Sreg {0};
    }
    static constexpr Sreg cs() {
      return Sreg {1};
    }
    static constexpr Sreg ss() {
      return Sreg {2};
    }
    static constexpr Sreg ds() {
      return Sreg {3};
    }
    static constexpr Fs fs()   {
      return Fs {};
    }
    static constexpr Gs gs()   {
      return Gs {};
    }

    static constexpr St0 st0() {
      return St0 {};
    }
    static constexpr St st1()  {
      return St {1};
    }
    static constexpr St st2()  {
      return St {2};
    }
    static constexpr St st3()  {
      return St {3};
    }
    static constexpr St st4()  {
      return St {4};
    }
    static constexpr St st5()  {
      return St {5};
    }
    static constexpr St st6()  {
      return St {6};
    }
    static constexpr St st7()  {
      return St {7};
    }

    static constexpr Xmm0 xmm0()  {
      return Xmm0 {};
    }
    static constexpr Xmm xmm1()   {
      return Xmm {1};
    }
    static constexpr Xmm xmm2()   {
      return Xmm {2};
    }
    static constexpr Xmm xmm3()   {
      return Xmm {3};
    }
    static constexpr Xmm xmm4()   {
      return Xmm {4};
    }
    static constexpr Xmm xmm5()   {
      return Xmm {5};
    }
    static constexpr Xmm xmm6()   {
      return Xmm {6};
    }
    static constexpr Xmm xmm7()   {
      return Xmm {7};
    }
    static constexpr Xmm xmm8()   {
      return Xmm {8};
    }
    static constexpr Xmm xmm9()   {
      return Xmm {9};
    }
    static constexpr Xmm xmm10()  {
      return Xmm {10};
    }
    static constexpr Xmm xmm11()  {
      return Xmm {11};
    }
    static constexpr Xmm xmm12()  {
      return Xmm {12};
    }
    static constexpr Xmm xmm13()  {
      return Xmm {13};
    }
    static constexpr Xmm xmm14()  {
      return Xmm {14};
    }
    static constexpr Xmm xmm15()  {
      return Xmm {15};
    }

    static constexpr Ymm ymm0()  {
      return Ymm {0};
    }
    static constexpr Ymm ymm1()  {
      return Ymm {1};
    }
    static constexpr Ymm ymm2()  {
      return Ymm {2};
    }
    static constexpr Ymm ymm3()  {
      return Ymm {3};
    }
    static constexpr Ymm ymm4()  {
      return Ymm {4};
    }
    static constexpr Ymm ymm5()  {
      return Ymm {5};
    }
    static constexpr Ymm ymm6()  {
      return Ymm {6};
    }
    static constexpr Ymm ymm7()  {
      return Ymm {7};
    }
    static constexpr Ymm ymm8()  {
      return Ymm {8};
    }
    static constexpr Ymm ymm9()  {
      return Ymm {9};
    }
    static constexpr Ymm ymm10() {
      return Ymm {10};
    }
    static constexpr Ymm ymm11() {
      return Ymm {11};
    }
    static constexpr Ymm ymm12() {
      return Ymm {12};
    }
    static constexpr Ymm ymm13() {
      return Ymm {13};
    }
    static constexpr Ymm ymm14() {
      return Ymm {14};
    }
    static constexpr Ymm ymm15() {
      return Ymm {15};
    }
};

// Convenience definitions follow...

constexpr Eflags eflags_cf {
  Constants::eflags_cf()
};
constexpr Eflags eflags_pf {
  Constants::eflags_pf()
};
constexpr Eflags eflags_af {
  Constants::eflags_af()
};
constexpr Eflags eflags_zf {
  Constants::eflags_zf()
};
constexpr Eflags eflags_sf {
  Constants::eflags_sf()
};
constexpr Eflags eflags_tf {
  Constants::eflags_tf()
};
constexpr Eflags eflags_if {
  Constants::eflags_if()
};
constexpr Eflags eflags_df {
  Constants::eflags_df()
};
constexpr Eflags eflags_of {
  Constants::eflags_of()
};
constexpr Eflags eflags_iopl {
  Constants::eflags_iopl()
};
constexpr Eflags eflags_nt {
  Constants::eflags_nt()
};
constexpr Eflags eflags_rf {
  Constants::eflags_rf()
};
constexpr Eflags eflags_vm {
  Constants::eflags_vm()
};
constexpr Eflags eflags_ac {
  Constants::eflags_ac()
};
constexpr Eflags eflags_vif {
  Constants::eflags_vif()
};
constexpr Eflags eflags_vip {
  Constants::eflags_vip()
};
constexpr Eflags eflags_id {
  Constants::eflags_id()
};

constexpr Eflags eflags[17] {
  eflags_cf, eflags_pf,   eflags_af,  eflags_zf,
  eflags_sf, eflags_tf,   eflags_if,  eflags_df,
  eflags_of, eflags_iopl, eflags_nt,  eflags_rf,
  eflags_vm, eflags_ac,   eflags_vif, eflags_vip,
  eflags_id
};

constexpr FpuControl fpu_control_im {
  Constants::fpu_control_im()
};
constexpr FpuControl fpu_control_dm {
  Constants::fpu_control_dm()
};
constexpr FpuControl fpu_control_zm {
  Constants::fpu_control_zm()
};
constexpr FpuControl fpu_control_om {
  Constants::fpu_control_om()
};
constexpr FpuControl fpu_control_um {
  Constants::fpu_control_um()
};
constexpr FpuControl fpu_control_pm {
  Constants::fpu_control_pm()
};
constexpr FpuControl fpu_control_pc {
  Constants::fpu_control_pc()
};
constexpr FpuControl fpu_control_rc {
  Constants::fpu_control_rc()
};
constexpr FpuControl fpu_control_x {
  Constants::fpu_control_x()
};

constexpr FpuControl fpu_control[9] {
  fpu_control_im, fpu_control_dm, fpu_control_zm, fpu_control_om,
  fpu_control_um, fpu_control_pm, fpu_control_pc, fpu_control_rc,
  fpu_control_x
};

constexpr FpuStatus fpu_status_ie {
  Constants::fpu_status_ie()
};
constexpr FpuStatus fpu_status_de {
  Constants::fpu_status_de()
};
constexpr FpuStatus fpu_status_ze {
  Constants::fpu_status_ze()
};
constexpr FpuStatus fpu_status_oe {
  Constants::fpu_status_oe()
};
constexpr FpuStatus fpu_status_ue {
  Constants::fpu_status_ue()
};
constexpr FpuStatus fpu_status_pe {
  Constants::fpu_status_pe()
};
constexpr FpuStatus fpu_status_sf {
  Constants::fpu_status_sf()
};
constexpr FpuStatus fpu_status_es {
  Constants::fpu_status_es()
};
constexpr FpuStatus fpu_status_c0 {
  Constants::fpu_status_c0()
};
constexpr FpuStatus fpu_status_c1 {
  Constants::fpu_status_c1()
};
constexpr FpuStatus fpu_status_c2 {
  Constants::fpu_status_c2()
};
constexpr FpuStatus fpu_status_top {
  Constants::fpu_status_top()
};
constexpr FpuStatus fpu_status_c3 {
  Constants::fpu_status_c3()
};
constexpr FpuStatus fpu_status_b {
  Constants::fpu_status_b()
};

constexpr FpuStatus fpu_status[14] {
  fpu_status_ie, fpu_status_de, fpu_status_ze, fpu_status_oe,
  fpu_status_ue, fpu_status_pe, fpu_status_sf, fpu_status_es,
  fpu_status_c0, fpu_status_c1, fpu_status_c2, fpu_status_top,
  fpu_status_c3, fpu_status_b
};

constexpr FpuTag tag0 {
  Constants::tag0()
};
constexpr FpuTag tag1 {
  Constants::tag1()
};
constexpr FpuTag tag2 {
  Constants::tag2()
};
constexpr FpuTag tag3 {
  Constants::tag3()
};
constexpr FpuTag tag4 {
  Constants::tag4()
};
constexpr FpuTag tag5 {
  Constants::tag5()
};
constexpr FpuTag tag6 {
  Constants::tag6()
};
constexpr FpuTag tag7 {
  Constants::tag7()
};

constexpr FpuTag fpu_tags[8] {
  tag0, tag1, tag2, tag3, tag4, tag5, tag6, tag7
};

constexpr Mxcsr mxcsr_ie {
  Constants::mxcsr_ie()
};
constexpr Mxcsr mxcsr_de {
  Constants::mxcsr_de()
};
constexpr Mxcsr mxcsr_ze {
  Constants::mxcsr_ze()
};
constexpr Mxcsr mxcsr_oe {
  Constants::mxcsr_oe()
};
constexpr Mxcsr mxcsr_ue {
  Constants::mxcsr_ue()
};
constexpr Mxcsr mxcsr_pe {
  Constants::mxcsr_pe()
};
constexpr Mxcsr mxcsr_daz {
  Constants::mxcsr_daz()
};
constexpr Mxcsr mxcsr_im {
  Constants::mxcsr_im()
};
constexpr Mxcsr mxcsr_dm {
  Constants::mxcsr_dm()
};
constexpr Mxcsr mxcsr_zm {
  Constants::mxcsr_zm()
};
constexpr Mxcsr mxcsr_om {
  Constants::mxcsr_om()
};
constexpr Mxcsr mxcsr_um {
  Constants::mxcsr_um()
};
constexpr Mxcsr mxcsr_pm {
  Constants::mxcsr_pm()
};
constexpr Mxcsr mxcsr_rc {
  Constants::mxcsr_rc()
};
constexpr Mxcsr mxcsr_fz {
  Constants::mxcsr_fz()
};

constexpr Mxcsr mxcsr[15] {
  mxcsr_ie, mxcsr_de, mxcsr_ze,  mxcsr_oe,
  mxcsr_ue, mxcsr_pe, mxcsr_daz, mxcsr_im,
  mxcsr_dm, mxcsr_zm, mxcsr_om,  mxcsr_um,
  mxcsr_pm, mxcsr_rc, mxcsr_fz
};

constexpr FpuData fpu_data {
  Constants::fpu_data()
};
constexpr FpuInstruction fpu_instruction {
  Constants::fpu_instruction()
};
constexpr FpuOpcode fpu_opcode {
  Constants::fpu_opcode()
};
constexpr Rip rip {
  Constants::rip()
};

constexpr Hint taken {
  Constants::taken()
};
constexpr Hint not_taken {
  Constants::not_taken()
};

constexpr Zero zero {
  Constants::zero()
};
constexpr One one {
  Constants::one()
};
constexpr Three three {
  Constants::three()
};

constexpr Mm mm0 {
  Constants::mm0()
};
constexpr Mm mm1 {
  Constants::mm1()
};
constexpr Mm mm2 {
  Constants::mm2()
};
constexpr Mm mm3 {
  Constants::mm3()
};
constexpr Mm mm4 {
  Constants::mm4()
};
constexpr Mm mm5 {
  Constants::mm5()
};
constexpr Mm mm6 {
  Constants::mm6()
};
constexpr Mm mm7 {
  Constants::mm7()
};

constexpr Mm mms[8] {
  mm0, mm1, mm2, mm3, mm4, mm5, mm6, mm7
};

constexpr Pref66 pref_66 {
  Constants::pref_66()
};
constexpr PrefRexW pref_rex_w {
  Constants::pref_rex_w()
};
constexpr Far far {
  Constants::far()
};

constexpr Al al {
  Constants::al()
};
constexpr Cl cl {
  Constants::cl()
};
constexpr Rl dl {
  Constants::dl()
};
constexpr Rl bl {
  Constants::bl()
};

constexpr Rl rls[4] {
  al, cl, dl, bl
};

constexpr Rh ah {
  Constants::ah()
};
constexpr Rh ch {
  Constants::ch()
};
constexpr Rh dh {
  Constants::dh()
};
constexpr Rh bh {
  Constants::bh()
};

constexpr Rh rhs[4] {
  ah, ch, dh, bh
};

constexpr Rb spl {
  Constants::spl()
};
constexpr Rb bpl {
  Constants::bpl()
};
constexpr Rb sil {
  Constants::sil()
};
constexpr Rb dil {
  Constants::dil()
};
constexpr Rb r8b {
  Constants::r8b()
};
constexpr Rb r9b {
  Constants::r9b()
};
constexpr Rb r10b {
  Constants::r10b()
};
constexpr Rb r11b {
  Constants::r11b()
};
constexpr Rb r12b {
  Constants::r12b()
};
constexpr Rb r13b {
  Constants::r13b()
};
constexpr Rb r14b {
  Constants::r14b()
};
constexpr Rb r15b {
  Constants::r15b()
};

constexpr Rb rbs[12] {
  spl, bpl, sil, dil, r8b, r9b, r10b, r11b, r12b, r13b, r14b, r15b
};

constexpr Ax ax {
  Constants::ax()
};
constexpr R16 cx {
  Constants::cx()
};
constexpr Dx dx {
  Constants::dx()
};
constexpr R16 bx {
  Constants::bx()
};
constexpr R16 sp {
  Constants::sp()
};
constexpr R16 bp {
  Constants::bp()
};
constexpr R16 si {
  Constants::si()
};
constexpr R16 di {
  Constants::di()
};
constexpr R16 r8w {
  Constants::r8w()
};
constexpr R16 r9w {
  Constants::r9w()
};
constexpr R16 r10w {
  Constants::r10w()
};
constexpr R16 r11w {
  Constants::r11w()
};
constexpr R16 r12w {
  Constants::r12w()
};
constexpr R16 r13w {
  Constants::r13w()
};
constexpr R16 r14w {
  Constants::r14w()
};
constexpr R16 r15w {
  Constants::r15w()
};

constexpr R16 r16s[16] {
  ax,  cx,  dx,   bx,   sp,   bp,   si,   di,
  r8w, r9w, r10w, r11w, r12w, r13w, r14w, r15w
};

constexpr Eax eax {
  Constants::eax()
};
constexpr R32 ecx {
  Constants::ecx()
};
constexpr R32 edx {
	Constants::edx()
};
constexpr R32 ebx {
  Constants::ebx()
};
constexpr R32 esp {
  Constants::esp()
};
constexpr R32 ebp {
  Constants::ebp()
};
constexpr R32 esi {
  Constants::esi()
};
constexpr R32 edi {
  Constants::edi()
};
constexpr R32 r8d {
  Constants::r8d()
};
constexpr R32 r9d {
  Constants::r9d()
};
constexpr R32 r10d {
  Constants::r10d()
};
constexpr R32 r11d {
  Constants::r11d()
};
constexpr R32 r12d {
  Constants::r12d()
};
constexpr R32 r13d {
  Constants::r13d()
};
constexpr R32 r14d {
  Constants::r14d()
};
constexpr R32 r15d {
  Constants::r15d()
};

constexpr R32 r32s[16] {
  eax, ecx, edx,  ebx,  esp,  ebp,  esi,  edi,
  r8d, r9d, r10d, r11d, r12d, r13d, r14d, r15d
};

constexpr Rax rax {
  Constants::rax()
};
constexpr R64 rcx {
  Constants::rcx()
};
constexpr R64 rdx {
  Constants::rdx()
};
constexpr R64 rbx {
  Constants::rbx()
};
constexpr R64 rsp {
  Constants::rsp()
};
constexpr R64 rbp {
  Constants::rbp()
};
constexpr R64 rsi {
  Constants::rsi()
};
constexpr R64 rdi {
  Constants::rdi()
};
constexpr R64 r8 {
  Constants::r8()
};
constexpr R64 r9 {
  Constants::r9()
};
constexpr R64 r10 {
  Constants::r10()
};
constexpr R64 r11 {
  Constants::r11()
};
constexpr R64 r12 {
  Constants::r12()
};
constexpr R64 r13 {
  Constants::r13()
};
constexpr R64 r14 {
  Constants::r14()
};
constexpr R64 r15 {
  Constants::r15()
};

constexpr R64 r64s[16] {
  rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi,
  r8,  r9,  r10, r11, r12, r13, r14, r15
};

constexpr Sreg es {
  Constants::es()
};
constexpr Sreg cs {
  Constants::cs()
};
constexpr Sreg ss {
  Constants::ss()
};
constexpr Sreg ds {
  Constants::ds()
};
constexpr Fs fs {
  Constants::fs()
};
constexpr Gs gs {
  Constants::gs()
};

constexpr Sreg sregs[6] {
  es, cs, ss, ds, fs, gs
};

constexpr St0 st0 {
  Constants::st0()
};
constexpr St st1 {
  Constants::st1()
};
constexpr St st2 {
  Constants::st2()
};
constexpr St st3 {
  Constants::st3()
};
constexpr St st4 {
  Constants::st4()
};
constexpr St st5 {
  Constants::st5()
};
constexpr St st6 {
  Constants::st6()
};
constexpr St st7 {
  Constants::st7()
};

constexpr St sts[8] {
  st0, st1, st2, st3, st4, st5, st6, st7
};

constexpr Xmm0 xmm0 {
  Constants::xmm0()
};
constexpr Xmm xmm1 {
  Constants::xmm1()
};
constexpr Xmm xmm2 {
  Constants::xmm2()
};
constexpr Xmm xmm3 {
  Constants::xmm3()
};
constexpr Xmm xmm4 {
  Constants::xmm4()
};
constexpr Xmm xmm5 {
  Constants::xmm5()
};
constexpr Xmm xmm6 {
  Constants::xmm6()
};
constexpr Xmm xmm7 {
  Constants::xmm7()
};
constexpr Xmm xmm8 {
  Constants::xmm8()
};
constexpr Xmm xmm9 {
  Constants::xmm9()
};
constexpr Xmm xmm10 {
  Constants::xmm10()
};
constexpr Xmm xmm11 {
  Constants::xmm11()
};
constexpr Xmm xmm12 {
  Constants::xmm12()
};
constexpr Xmm xmm13 {
  Constants::xmm13()
};
constexpr Xmm xmm14 {
  Constants::xmm14()
};
constexpr Xmm xmm15 {
  Constants::xmm15()
};

constexpr Xmm xmms[16] {
  xmm0, xmm1, xmm2,  xmm3,  xmm4,  xmm5,  xmm6,  xmm7,
  xmm8, xmm9, xmm10, xmm11, xmm12, xmm13, xmm14, xmm15
};

constexpr Ymm ymm0 {
  Constants::ymm0()
};
constexpr Ymm ymm1 {
  Constants::ymm1()
};
constexpr Ymm ymm2 {
  Constants::ymm2()
};
constexpr Ymm ymm3 {
  Constants::ymm3()
};
constexpr Ymm ymm4 {
  Constants::ymm4()
};
constexpr Ymm ymm5 {
  Constants::ymm5()
};
constexpr Ymm ymm6 {
  Constants::ymm6()
};
constexpr Ymm ymm7 {
  Constants::ymm7()
};
constexpr Ymm ymm8 {
  Constants::ymm8()
};
constexpr Ymm ymm9 {
  Constants::ymm9()
};
constexpr Ymm ymm10 {
  Constants::ymm10()
};
constexpr Ymm ymm11 {
  Constants::ymm11()
};
constexpr Ymm ymm12 {
  Constants::ymm12()
};
constexpr Ymm ymm13 {
  Constants::ymm13()
};
constexpr Ymm ymm14 {
  Constants::ymm14()
};
constexpr Ymm ymm15 {
  Constants::ymm15()
};

constexpr Ymm ymms[16] {
  ymm0, ymm1, ymm2,  ymm3,  ymm4,  ymm5,  ymm6,  ymm7,
  ymm8, ymm9, ymm10, ymm11, ymm12, ymm13, ymm14, ymm15
};
} // namespace x64asm

#endif
