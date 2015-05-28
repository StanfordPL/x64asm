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

#include <array>

namespace x64asm {

class Constants {
public:
  static constexpr Eflags eflags_cf()   {
    return {0, 1};
  }
  static constexpr Eflags eflags_res1() {
    return {1, 1};
  }
  static constexpr Eflags eflags_pf()   {
    return {2, 1};
  }
  static constexpr Eflags eflags_res3() {
    return {3, 1};
  }
  static constexpr Eflags eflags_af()   {
    return {4, 1};
  }
  static constexpr Eflags eflags_res5() {
    return {5, 1};
  }
  static constexpr Eflags eflags_zf()   {
    return {6, 1};
  }
  static constexpr Eflags eflags_sf()   {
    return {7, 1};
  }
  static constexpr Eflags eflags_tf()   {
    return {8, 1};
  }
  static constexpr Eflags eflags_if()   {
    return {9, 1};
  }
  static constexpr Eflags eflags_df()   {
    return {10, 1};
  }
  static constexpr Eflags eflags_of()   {
    return {11, 1};
  }
  static constexpr Eflags eflags_iopl() {
    return {12, 2};
  }
  static constexpr Eflags eflags_nt()   {
    return {14, 1};
  }
  static constexpr Eflags eflags_res15() {
    return {15, 1};
  }
  static constexpr Eflags eflags_rf()   {
    return {16, 1};
  }
  static constexpr Eflags eflags_vm()   {
    return {17, 1};
  }
  static constexpr Eflags eflags_ac()   {
    return {18, 1};
  }
  static constexpr Eflags eflags_vif()  {
    return {19, 1};
  }
  static constexpr Eflags eflags_vip()  {
    return {20, 1};
  }
  static constexpr Eflags eflags_id()   {
    return {21, 1};
  }
  static constexpr std::array<Eflags,21> eflags() {
    return {
      eflags_cf(),   eflags_res1(), eflags_pf(),    eflags_res3(),
      eflags_af(),   eflags_res5(), eflags_zf(),    eflags_sf(),
      eflags_tf(),   eflags_if(),   eflags_df(),    eflags_of(),
      eflags_iopl(), eflags_nt(),   eflags_res15(), eflags_rf(),
      eflags_vm(),   eflags_ac(),   eflags_vif(),   eflags_vip(),
      eflags_id()
    };
  }

  static constexpr FpuControl fpu_control_im() {
    return {0, 1};
  }
  static constexpr FpuControl fpu_control_dm() {
    return {1, 1};
  }
  static constexpr FpuControl fpu_control_zm() {
    return {2, 1};
  }
  static constexpr FpuControl fpu_control_om() {
    return {3, 1};
  }
  static constexpr FpuControl fpu_control_um() {
    return {4, 1};
  }
  static constexpr FpuControl fpu_control_pm() {
    return {5, 1};
  }
  static constexpr FpuControl fpu_control_res6() {
    return {6, 1};
  }
  static constexpr FpuControl fpu_control_res7() {
    return {7, 1};
  }
  static constexpr FpuControl fpu_control_pc() {
    return {8, 2};
  }
  static constexpr FpuControl fpu_control_rc() {
    return {10, 2};
  }
  static constexpr FpuControl fpu_control_x()  {
    return {12, 1};
  }
  static constexpr FpuControl fpu_control_res13() {
    return {13, 1};
  }
  static constexpr FpuControl fpu_control_res14() {
    return {14, 1};
  }
  static constexpr FpuControl fpu_control_res15() {
    return {15, 1};
  }
  static constexpr std::array<FpuControl,14> fpu_control() {
    return {
      fpu_control_im(),    fpu_control_dm(),   fpu_control_zm(),   fpu_control_om(),
      fpu_control_um(),    fpu_control_pm(),   fpu_control_res6(), fpu_control_res7(),
      fpu_control_pc(),    fpu_control_rc(),   fpu_control_x(),    fpu_control_res13(),
      fpu_control_res14(), fpu_control_res15()
    };
  }

  static constexpr FpuStatus fpu_status_ie()  {
    return {0, 1};
  }
  static constexpr FpuStatus fpu_status_de()  {
    return {1, 1};
  }
  static constexpr FpuStatus fpu_status_ze()  {
    return {2, 1};
  }
  static constexpr FpuStatus fpu_status_oe()  {
    return {3, 1};
  }
  static constexpr FpuStatus fpu_status_ue()  {
    return {4, 1};
  }
  static constexpr FpuStatus fpu_status_pe()  {
    return {5, 1};
  }
  static constexpr FpuStatus fpu_status_sf()  {
    return {6, 1};
  }
  static constexpr FpuStatus fpu_status_es()  {
    return {7, 1};
  }
  static constexpr FpuStatus fpu_status_c0()  {
    return {8, 1};
  }
  static constexpr FpuStatus fpu_status_c1()  {
    return {9, 1};
  }
  static constexpr FpuStatus fpu_status_c2()  {
    return {10, 1};
  }
  static constexpr FpuStatus fpu_status_top() {
    return {11, 3};
  }
  static constexpr FpuStatus fpu_status_c3()  {
    return {14, 1};
  }
  static constexpr FpuStatus fpu_status_b()   {
    return {15, 1};
  }
  static constexpr std::array<FpuStatus,14> fpu_status() {
    return {
      fpu_status_ie(), fpu_status_de(), fpu_status_ze(), fpu_status_oe(),
      fpu_status_ue(), fpu_status_pe(), fpu_status_sf(), fpu_status_es(),
      fpu_status_c0(), fpu_status_c1(), fpu_status_c2(), fpu_status_top(),
      fpu_status_c3(), fpu_status_b()
    };
  }

  static constexpr FpuTag tag0() {
    return {0, 2};
  }
  static constexpr FpuTag tag1() {
    return {2, 2};
  }
  static constexpr FpuTag tag2() {
    return {4, 2};
  }
  static constexpr FpuTag tag3() {
    return {6, 2};
  }
  static constexpr FpuTag tag4() {
    return {8, 2};
  }
  static constexpr FpuTag tag5() {
    return {10, 2};
  }
  static constexpr FpuTag tag6() {
    return {12, 2};
  }
  static constexpr FpuTag tag7() {
    return {14, 2};
  }
  static constexpr std::array<FpuTag,8> fpu_tags() {
    return {
      tag0(), tag1(), tag2(), tag3(), tag4(), tag5(), tag6(), tag7()
    };
  }

  static constexpr Mxcsr mxcsr_ie()  {
    return {0, 1};
  }
  static constexpr Mxcsr mxcsr_de()  {
    return {1, 1};
  }
  static constexpr Mxcsr mxcsr_ze()  {
    return {2, 1};
  }
  static constexpr Mxcsr mxcsr_oe()  {
    return {3, 1};
  }
  static constexpr Mxcsr mxcsr_ue()  {
    return {4, 1};
  }
  static constexpr Mxcsr mxcsr_pe()  {
    return {5, 1};
  }
  static constexpr Mxcsr mxcsr_daz() {
    return {6, 1};
  }
  static constexpr Mxcsr mxcsr_im()  {
    return {7, 1};
  }
  static constexpr Mxcsr mxcsr_dm()  {
    return {8, 1};
  }
  static constexpr Mxcsr mxcsr_zm()  {
    return {9, 1};
  }
  static constexpr Mxcsr mxcsr_om()  {
    return {10, 1};
  }
  static constexpr Mxcsr mxcsr_um()  {
    return {11, 1};
  }
  static constexpr Mxcsr mxcsr_pm()  {
    return {12, 1};
  }
  static constexpr Mxcsr mxcsr_rc()  {
    return {13, 2};
  }
  static constexpr Mxcsr mxcsr_fz()  {
    return {15, 1};
  }
  static constexpr std::array<Mxcsr,15> mxcsr() {
    return {
      mxcsr_ie(), mxcsr_de(), mxcsr_ze(),  mxcsr_oe(),
      mxcsr_ue(), mxcsr_pe(), mxcsr_daz(), mxcsr_im(),
      mxcsr_dm(), mxcsr_zm(), mxcsr_om(),  mxcsr_um(),
      mxcsr_pm(), mxcsr_rc(), mxcsr_fz()
    };
  }

  static constexpr Rip rip() {
    return {};
  }
  static constexpr FpuData fpu_data() {
    return {};
  }
  static constexpr FpuInstruction fpu_instruction() {
    return {};
  }
  static constexpr FpuOpcode fpu_opcode() {
    return {};
  }

  static constexpr Hint taken() {
    return {0};
  }
  static constexpr Hint not_taken() {
    return {1};
  }

  static constexpr Zero zero() {
    return {};
  }
  static constexpr One one() {
    return {};
  }
  static constexpr Three three() {
    return {};
  }

  static constexpr Mm mm0() {
    return {0};
  }
  static constexpr Mm mm1() {
    return {1};
  }
  static constexpr Mm mm2() {
    return {2};
  }
  static constexpr Mm mm3() {
    return {3};
  }
  static constexpr Mm mm4() {
    return {4};
  }
  static constexpr Mm mm5() {
    return {5};
  }
  static constexpr Mm mm6() {
    return {6};
  }
  static constexpr Mm mm7() {
    return {7};
  }
  static constexpr std::array<Mm,8> mms() {
    return {
      mm0(), mm1(), mm2(), mm3(), mm4(), mm5(), mm6(), mm7()
    };
  }

  static constexpr Pref66 pref_66() {
    return {};
  }
  static constexpr PrefRexW pref_rex_w() {
    return {};
  }
  static constexpr Far far() {
    return {};
  }

  static constexpr Al al() {
    return {};
  }
  static constexpr Cl cl() {
    return {};
  }
  static constexpr R8 dl() {
    return {2};
  }
  static constexpr R8 bl() {
    return {3};
  }
  static constexpr R8 spl()  {
    return {4};
  }
  static constexpr R8 bpl()  {
    return {5};
  }
  static constexpr R8 sil()  {
    return {6};
  }
  static constexpr R8 dil()  {
    return {7};
  }
  static constexpr R8 r8b()  {
    return {8};
  }
  static constexpr R8 r9b()  {
    return {9};
  }
  static constexpr R8 r10b() {
    return {10};
  }
  static constexpr R8 r11b() {
    return {11};
  }
  static constexpr R8 r12b() {
    return {12};
  }
  static constexpr R8 r13b() {
    return {13};
  }
  static constexpr R8 r14b() {
    return {14};
  }
  static constexpr R8 r15b() {
    return {15};
  }
  static constexpr std::array<R8, 16> r8s() {
    return {
      al(), cl(), dl(), bl(),
      spl(), bpl(), sil(), dil(), 
      r8b(), r9b(), r10b(), r11b(), 
      r12b(), r13b(), r14b(), r15b()
    };
  }

  static constexpr Rh ah() {
    return {4};
  }
  static constexpr Rh ch() {
    return {5};
  }
  static constexpr Rh dh() {
    return {6};
  }
  static constexpr Rh bh() {
    return {7};
  }
  static constexpr std::array<Rh,4> rhs() {
    return {
      ah(), ch(), dh(), bh()
    };
  }

  static constexpr Ax ax()    {
    return {};
  }
  static constexpr R16 cx()   {
    return {1};
  }
  static constexpr Dx dx()    {
    return {};
  }
  static constexpr R16 bx()   {
    return {3};
  }
  static constexpr R16 sp()   {
    return {4};
  }
  static constexpr R16 bp()   {
    return {5};
  }
  static constexpr R16 si()   {
    return {6};
  }
  static constexpr R16 di()   {
    return {7};
  }
  static constexpr R16 r8w()  {
    return {8};
  }
  static constexpr R16 r9w()  {
    return {9};
  }
  static constexpr R16 r10w() {
    return {10};
  }
  static constexpr R16 r11w() {
    return {11};
  }
  static constexpr R16 r12w() {
    return {12};
  }
  static constexpr R16 r13w() {
    return {13};
  }
  static constexpr R16 r14w() {
    return {14};
  }
  static constexpr R16 r15w() {
    return {15};
  }
  static constexpr std::array<R16, 16> r16s() {
    return {
      ax(),  cx(),  dx(),   bx(),   sp(),   bp(),   si(),   di(),
      r8w(), r9w(), r10w(), r11w(), r12w(), r13w(), r14w(), r15w()
    };
  }

  static constexpr Eax eax()  {
    return {};
  }
  static constexpr R32 ecx()  {
    return {1};
  }
  static constexpr R32 edx()  {
    return {2};
  }
  static constexpr R32 ebx()  {
    return {3};
  }
  static constexpr R32 esp()  {
    return {4};
  }
  static constexpr R32 ebp()  {
    return {5};
  }
  static constexpr R32 esi()  {
    return {6};
  }
  static constexpr R32 edi()  {
    return {7};
  }
  static constexpr R32 r8d()  {
    return {8};
  }
  static constexpr R32 r9d()  {
    return {9};
  }
  static constexpr R32 r10d() {
    return {10};
  }
  static constexpr R32 r11d() {
    return {11};
  }
  static constexpr R32 r12d() {
    return {12};
  }
  static constexpr R32 r13d() {
    return {13};
  }
  static constexpr R32 r14d() {
    return {14};
  }
  static constexpr R32 r15d() {
    return {15};
  }
  static constexpr std::array<R32,16> r32s() {
    return {
      eax(), ecx(), edx(),  ebx(),  esp(),  ebp(),  esi(),  edi(),
      r8d(), r9d(), r10d(), r11d(), r12d(), r13d(), r14d(), r15d()
    };
  }

  static constexpr Rax rax() {
    return {};
  }
  static constexpr R64 rcx() {
    return {1};
  }
  static constexpr R64 rdx() {
    return {2};
  }
  static constexpr R64 rbx() {
    return {3};
  }
  static constexpr R64 rsp() {
    return {4};
  }
  static constexpr R64 rbp() {
    return {5};
  }
  static constexpr R64 rsi() {
    return {6};
  }
  static constexpr R64 rdi() {
    return {7};
  }
  static constexpr R64 r8()  {
    return {8};
  }
  static constexpr R64 r9()  {
    return {9};
  }
  static constexpr R64 r10() {
    return {10};
  }
  static constexpr R64 r11() {
    return {11};
  }
  static constexpr R64 r12() {
    return {12};
  }
  static constexpr R64 r13() {
    return {13};
  }
  static constexpr R64 r14() {
    return {14};
  }
  static constexpr R64 r15() {
    return {15};
  }
  static constexpr std::array<R64, 16> r64s() {
    return {
      rax(), rcx(), rdx(), rbx(), rsp(), rbp(), rsi(), rdi(),
      r8(),  r9(),  r10(), r11(), r12(), r13(), r14(), r15()
    };
  }

  static constexpr Sreg es() {
    return {0};
  }
  static constexpr Sreg cs() {
    return {1};
  }
  static constexpr Sreg ss() {
    return {2};
  }
  static constexpr Sreg ds() {
    return {3};
  }
  static constexpr Fs fs()   {
    return {};
  }
  static constexpr Gs gs()   {
    return {};
  }
  static constexpr std::array<Sreg, 6> sregs() {
    return {
      es(), cs(), ss(), ds(), fs(), gs()
    };
  }

  static constexpr St0 st0() {
    return {};
  }
  static constexpr St st1()  {
    return {1};
  }
  static constexpr St st2()  {
    return {2};
  }
  static constexpr St st3()  {
    return {3};
  }
  static constexpr St st4()  {
    return {4};
  }
  static constexpr St st5()  {
    return {5};
  }
  static constexpr St st6()  {
    return {6};
  }
  static constexpr St st7()  {
    return {7};
  }
  static constexpr std::array<St,8> sts() {
    return {
      st0(), st1(), st2(), st3(), st4(), st5(), st6(), st7()
    };
  }

  static constexpr Xmm0 xmm0()  {
    return {};
  }
  static constexpr Xmm xmm1()   {
    return {1};
  }
  static constexpr Xmm xmm2()   {
    return {2};
  }
  static constexpr Xmm xmm3()   {
    return {3};
  }
  static constexpr Xmm xmm4()   {
    return {4};
  }
  static constexpr Xmm xmm5()   {
    return {5};
  }
  static constexpr Xmm xmm6()   {
    return {6};
  }
  static constexpr Xmm xmm7()   {
    return {7};
  }
  static constexpr Xmm xmm8()   {
    return {8};
  }
  static constexpr Xmm xmm9()   {
    return {9};
  }
  static constexpr Xmm xmm10()  {
    return {10};
  }
  static constexpr Xmm xmm11()  {
    return {11};
  }
  static constexpr Xmm xmm12()  {
    return {12};
  }
  static constexpr Xmm xmm13()  {
    return {13};
  }
  static constexpr Xmm xmm14()  {
    return {14};
  }
  static constexpr Xmm xmm15()  {
    return {15};
  }
  static constexpr std::array<Xmm,16> xmms() {
    return {
      xmm0(), xmm1(), xmm2(),  xmm3(),  xmm4(),  xmm5(),  xmm6(),  xmm7(),
      xmm8(), xmm9(), xmm10(), xmm11(), xmm12(), xmm13(), xmm14(), xmm15()
    };
  }

  static constexpr Ymm ymm0()  {
    return {0};
  }
  static constexpr Ymm ymm1()  {
    return {1};
  }
  static constexpr Ymm ymm2()  {
    return {2};
  }
  static constexpr Ymm ymm3()  {
    return {3};
  }
  static constexpr Ymm ymm4()  {
    return {4};
  }
  static constexpr Ymm ymm5()  {
    return {5};
  }
  static constexpr Ymm ymm6()  {
    return {6};
  }
  static constexpr Ymm ymm7()  {
    return {7};
  }
  static constexpr Ymm ymm8()  {
    return {8};
  }
  static constexpr Ymm ymm9()  {
    return {9};
  }
  static constexpr Ymm ymm10() {
    return {10};
  }
  static constexpr Ymm ymm11() {
    return {11};
  }
  static constexpr Ymm ymm12() {
    return {12};
  }
  static constexpr Ymm ymm13() {
    return {13};
  }
  static constexpr Ymm ymm14() {
    return {14};
  }
  static constexpr Ymm ymm15() {
    return {15};
  }
  static constexpr std::array<Ymm,16> ymms() {
    return {
      ymm0(), ymm1(), ymm2(),  ymm3(),  ymm4(),  ymm5(),  ymm6(),  ymm7(),
      ymm8(), ymm9(), ymm10(), ymm11(), ymm12(), ymm13(), ymm14(), ymm15()
    };
  }
};

constexpr auto eflags_cf = Constants::eflags_cf();
constexpr auto eflags_res1 = Constants::eflags_res1();
constexpr auto eflags_pf = Constants::eflags_pf();
constexpr auto eflags_res3 = Constants::eflags_res3();
constexpr auto eflags_af = Constants::eflags_af();
constexpr auto eflags_res5 = Constants::eflags_res5();
constexpr auto eflags_zf = Constants::eflags_zf();
constexpr auto eflags_sf = Constants::eflags_sf();
constexpr auto eflags_tf = Constants::eflags_tf();
constexpr auto eflags_if = Constants::eflags_if();
constexpr auto eflags_df = Constants::eflags_df();
constexpr auto eflags_of = Constants::eflags_of();
constexpr auto eflags_iopl = Constants::eflags_iopl();
constexpr auto eflags_nt = Constants::eflags_nt();
constexpr auto eflags_res15 = Constants::eflags_res15();
constexpr auto eflags_rf = Constants::eflags_rf();
constexpr auto eflags_vm = Constants::eflags_vm();
constexpr auto eflags_ac = Constants::eflags_ac();
constexpr auto eflags_vif = Constants::eflags_vif();
constexpr auto eflags_vip = Constants::eflags_vip();
constexpr auto eflags_id = Constants::eflags_id();
constexpr auto eflags = Constants::eflags();

constexpr auto fpu_control_im = Constants::fpu_control_im();
constexpr auto fpu_control_dm = Constants::fpu_control_dm();
constexpr auto fpu_control_zm = Constants::fpu_control_zm();
constexpr auto fpu_control_om = Constants::fpu_control_om();
constexpr auto fpu_control_um = Constants::fpu_control_um();
constexpr auto fpu_control_pm = Constants::fpu_control_pm();
constexpr auto fpu_control_res6 = Constants::fpu_control_res6();
constexpr auto fpu_control_res7 = Constants::fpu_control_res7();
constexpr auto fpu_control_pc = Constants::fpu_control_pc();
constexpr auto fpu_control_rc = Constants::fpu_control_rc();
constexpr auto fpu_control_x = Constants::fpu_control_x();
constexpr auto fpu_control_res13 = Constants::fpu_control_res13();
constexpr auto fpu_control_res14 = Constants::fpu_control_res14();
constexpr auto fpu_control_res15 = Constants::fpu_control_res15();
constexpr auto fpu_control = Constants::fpu_control();

constexpr auto fpu_status_ie = Constants::fpu_status_ie();
constexpr auto fpu_status_de = Constants::fpu_status_de();
constexpr auto fpu_status_ze = Constants::fpu_status_ze();
constexpr auto fpu_status_oe = Constants::fpu_status_oe();
constexpr auto fpu_status_ue = Constants::fpu_status_ue();
constexpr auto fpu_status_pe = Constants::fpu_status_pe();
constexpr auto fpu_status_sf = Constants::fpu_status_sf();
constexpr auto fpu_status_es = Constants::fpu_status_es();
constexpr auto fpu_status_c0 = Constants::fpu_status_c0();
constexpr auto fpu_status_c1 = Constants::fpu_status_c1();
constexpr auto fpu_status_c2 = Constants::fpu_status_c2();
constexpr auto fpu_status_top = Constants::fpu_status_top();
constexpr auto fpu_status_c3 = Constants::fpu_status_c3();
constexpr auto fpu_status_b = Constants::fpu_status_b();
constexpr auto fpu_status = Constants::fpu_status();

constexpr auto tag0 = Constants::tag0();
constexpr auto tag1 = Constants::tag1();
constexpr auto tag2 = Constants::tag2();
constexpr auto tag3 = Constants::tag3();
constexpr auto tag4 = Constants::tag4();
constexpr auto tag5 = Constants::tag5();
constexpr auto tag6 = Constants::tag6();
constexpr auto tag7 = Constants::tag7();
constexpr auto fpu_tags = Constants::fpu_tags();

constexpr auto mxcsr_ie = Constants::mxcsr_ie();
constexpr auto mxcsr_de = Constants::mxcsr_de();
constexpr auto mxcsr_ze = Constants::mxcsr_ze();
constexpr auto mxcsr_oe = Constants::mxcsr_oe();
constexpr auto mxcsr_ue = Constants::mxcsr_ue();
constexpr auto mxcsr_pe = Constants::mxcsr_pe();
constexpr auto mxcsr_daz = Constants::mxcsr_daz();
constexpr auto mxcsr_im = Constants::mxcsr_im();
constexpr auto mxcsr_dm = Constants::mxcsr_dm();
constexpr auto mxcsr_zm = Constants::mxcsr_zm();
constexpr auto mxcsr_om = Constants::mxcsr_om();
constexpr auto mxcsr_um = Constants::mxcsr_um();
constexpr auto mxcsr_pm = Constants::mxcsr_pm();
constexpr auto mxcsr_rc = Constants::mxcsr_rc();
constexpr auto mxcsr_fz = Constants::mxcsr_fz();
constexpr auto mxcsr = Constants::mxcsr();

constexpr auto fpu_data = Constants::fpu_data();
constexpr auto fpu_instruction = Constants::fpu_instruction();
constexpr auto fpu_opcode = Constants::fpu_opcode();
constexpr auto rip = Constants::rip();

constexpr auto taken = Constants::taken();
constexpr auto not_taken = Constants::not_taken();

constexpr auto zero = Constants::zero();
constexpr auto one = Constants::one();
constexpr auto three = Constants::three();

constexpr auto mm0 = Constants::mm0();
constexpr auto mm1 = Constants::mm1();
constexpr auto mm2 = Constants::mm2();
constexpr auto mm3 = Constants::mm3();
constexpr auto mm4 = Constants::mm4();
constexpr auto mm5 = Constants::mm5();
constexpr auto mm6 = Constants::mm6();
constexpr auto mm7 = Constants::mm7();
constexpr auto mms = Constants::mms();

constexpr auto pref_66 = Constants::pref_66();
constexpr auto pref_rex_w = Constants::pref_rex_w();
constexpr auto far = Constants::far();

constexpr auto al = Constants::al();
constexpr auto cl = Constants::cl();
constexpr auto dl = Constants::dl();
constexpr auto bl = Constants::bl();
constexpr auto spl = Constants::spl();
constexpr auto bpl = Constants::bpl();
constexpr auto sil = Constants::sil();
constexpr auto dil = Constants::dil();
constexpr auto r8b = Constants::r8b();
constexpr auto r9b = Constants::r9b();
constexpr auto r10b = Constants::r10b();
constexpr auto r11b = Constants::r11b();
constexpr auto r12b = Constants::r12b();
constexpr auto r13b = Constants::r13b();
constexpr auto r14b = Constants::r14b();
constexpr auto r15b = Constants::r15b();
constexpr auto r8s = Constants::r8s();

constexpr auto ah = Constants::ah();
constexpr auto ch = Constants::ch();
constexpr auto dh = Constants::dh();
constexpr auto bh = Constants::bh();
constexpr auto rhs = Constants::rhs();

constexpr auto ax = Constants::ax();
constexpr auto cx = Constants::cx();
constexpr auto dx = Constants::dx();
constexpr auto bx = Constants::bx();
constexpr auto sp = Constants::sp();
constexpr auto bp = Constants::bp();
constexpr auto si = Constants::si();
constexpr auto di = Constants::di();
constexpr auto r8w = Constants::r8w();
constexpr auto r9w = Constants::r9w();
constexpr auto r10w = Constants::r10w();
constexpr auto r11w = Constants::r11w();
constexpr auto r12w = Constants::r12w();
constexpr auto r13w = Constants::r13w();
constexpr auto r14w = Constants::r14w();
constexpr auto r15w = Constants::r15w();
constexpr auto r16s = Constants::r16s();

constexpr auto eax = Constants::eax();
constexpr auto ecx = Constants::ecx();
constexpr auto edx = Constants::edx();
constexpr auto ebx = Constants::ebx();
constexpr auto esp = Constants::esp();
constexpr auto ebp = Constants::ebp();
constexpr auto esi = Constants::esi();
constexpr auto edi = Constants::edi();
constexpr auto r8d = Constants::r8d();
constexpr auto r9d = Constants::r9d();
constexpr auto r10d = Constants::r10d();
constexpr auto r11d = Constants::r11d();
constexpr auto r12d = Constants::r12d();
constexpr auto r13d = Constants::r13d();
constexpr auto r14d = Constants::r14d();
constexpr auto r15d = Constants::r15d();
constexpr auto r32s = Constants::r32s();

constexpr auto rax = Constants::rax();
constexpr auto rcx = Constants::rcx();
constexpr auto rdx = Constants::rdx();
constexpr auto rbx = Constants::rbx();
constexpr auto rsp = Constants::rsp();
constexpr auto rbp = Constants::rbp();
constexpr auto rsi = Constants::rsi();
constexpr auto rdi = Constants::rdi();
constexpr auto r8 = Constants::r8();
constexpr auto r9 = Constants::r9();
constexpr auto r10 = Constants::r10();
constexpr auto r11 = Constants::r11();
constexpr auto r12 = Constants::r12();
constexpr auto r13 = Constants::r13();
constexpr auto r14 = Constants::r14();
constexpr auto r15 = Constants::r15();
constexpr auto r64s = Constants::r64s();

constexpr auto es = Constants::es();
constexpr auto cs = Constants::cs();
constexpr auto ss = Constants::ss();
constexpr auto ds = Constants::ds();
constexpr auto fs = Constants::fs();
constexpr auto gs = Constants::gs();
constexpr auto sregs = Constants::sregs();

constexpr auto st0 = Constants::st0();
constexpr auto st1 = Constants::st1();
constexpr auto st2 = Constants::st2();
constexpr auto st3 = Constants::st3();
constexpr auto st4 = Constants::st4();
constexpr auto st5 = Constants::st5();
constexpr auto st6 = Constants::st6();
constexpr auto st7 = Constants::st7();
constexpr auto sts = Constants::sts();

constexpr auto xmm0 = Constants::xmm0();
constexpr auto xmm1 = Constants::xmm1();
constexpr auto xmm2 = Constants::xmm2();
constexpr auto xmm3 = Constants::xmm3();
constexpr auto xmm4 = Constants::xmm4();
constexpr auto xmm5 = Constants::xmm5();
constexpr auto xmm6 = Constants::xmm6();
constexpr auto xmm7 = Constants::xmm7();
constexpr auto xmm8 = Constants::xmm8();
constexpr auto xmm9 = Constants::xmm9();
constexpr auto xmm10 = Constants::xmm10();
constexpr auto xmm11 = Constants::xmm11();
constexpr auto xmm12 = Constants::xmm12();
constexpr auto xmm13 = Constants::xmm13();
constexpr auto xmm14 = Constants::xmm14();
constexpr auto xmm15 = Constants::xmm15();
constexpr auto xmms = Constants::xmms();

constexpr auto ymm0 = Constants::ymm0();
constexpr auto ymm1 = Constants::ymm1();
constexpr auto ymm2 = Constants::ymm2();
constexpr auto ymm3 = Constants::ymm3();
constexpr auto ymm4 = Constants::ymm4();
constexpr auto ymm5 = Constants::ymm5();
constexpr auto ymm6 = Constants::ymm6();
constexpr auto ymm7 = Constants::ymm7();
constexpr auto ymm8 = Constants::ymm8();
constexpr auto ymm9 = Constants::ymm9();
constexpr auto ymm10 = Constants::ymm10();
constexpr auto ymm11 = Constants::ymm11();
constexpr auto ymm12 = Constants::ymm12();
constexpr auto ymm13 = Constants::ymm13();
constexpr auto ymm14 = Constants::ymm14();
constexpr auto ymm15 = Constants::ymm15();
constexpr auto ymms = Constants::ymms();

} // namespace x64asm

#endif
