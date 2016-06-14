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
  static  Eflags eflags_cf()   {
    return {0, 1};
  }
  static  Eflags eflags_res1() {
    return {1, 1};
  }
  static  Eflags eflags_pf()   {
    return {2, 1};
  }
  static  Eflags eflags_res3() {
    return {3, 1};
  }
  static  Eflags eflags_af()   {
    return {4, 1};
  }
  static  Eflags eflags_res5() {
    return {5, 1};
  }
  static  Eflags eflags_zf()   {
    return {6, 1};
  }
  static  Eflags eflags_sf()   {
    return {7, 1};
  }
  static  Eflags eflags_tf()   {
    return {8, 1};
  }
  static  Eflags eflags_if()   {
    return {9, 1};
  }
  static  Eflags eflags_df()   {
    return {10, 1};
  }
  static  Eflags eflags_of()   {
    return {11, 1};
  }
  static  Eflags eflags_iopl() {
    return {12, 2};
  }
  static  Eflags eflags_nt()   {
    return {14, 1};
  }
  static  Eflags eflags_res15() {
    return {15, 1};
  }
  static  Eflags eflags_rf()   {
    return {16, 1};
  }
  static  Eflags eflags_vm()   {
    return {17, 1};
  }
  static  Eflags eflags_ac()   {
    return {18, 1};
  }
  static  Eflags eflags_vif()  {
    return {19, 1};
  }
  static  Eflags eflags_vip()  {
    return {20, 1};
  }
  static  Eflags eflags_id()   {
    return {21, 1};
  }
  static  std::array<Eflags,21> eflags() {
    return {
      eflags_cf(),   eflags_res1(), eflags_pf(),    eflags_res3(),
      eflags_af(),   eflags_res5(), eflags_zf(),    eflags_sf(),
      eflags_tf(),   eflags_if(),   eflags_df(),    eflags_of(),
      eflags_iopl(), eflags_nt(),   eflags_res15(), eflags_rf(),
      eflags_vm(),   eflags_ac(),   eflags_vif(),   eflags_vip(),
      eflags_id()
    };
  }

  static  FpuControl fpu_control_im() {
    return {0, 1};
  }
  static  FpuControl fpu_control_dm() {
    return {1, 1};
  }
  static  FpuControl fpu_control_zm() {
    return {2, 1};
  }
  static  FpuControl fpu_control_om() {
    return {3, 1};
  }
  static  FpuControl fpu_control_um() {
    return {4, 1};
  }
  static  FpuControl fpu_control_pm() {
    return {5, 1};
  }
  static  FpuControl fpu_control_res6() {
    return {6, 1};
  }
  static  FpuControl fpu_control_res7() {
    return {7, 1};
  }
  static  FpuControl fpu_control_pc() {
    return {8, 2};
  }
  static  FpuControl fpu_control_rc() {
    return {10, 2};
  }
  static  FpuControl fpu_control_x()  {
    return {12, 1};
  }
  static  FpuControl fpu_control_res13() {
    return {13, 1};
  }
  static  FpuControl fpu_control_res14() {
    return {14, 1};
  }
  static  FpuControl fpu_control_res15() {
    return {15, 1};
  }
  static  std::array<FpuControl,14> fpu_control() {
    return {
      fpu_control_im(),    fpu_control_dm(),   fpu_control_zm(),   fpu_control_om(),
      fpu_control_um(),    fpu_control_pm(),   fpu_control_res6(), fpu_control_res7(),
      fpu_control_pc(),    fpu_control_rc(),   fpu_control_x(),    fpu_control_res13(),
      fpu_control_res14(), fpu_control_res15()
    };
  }

  static  FpuStatus fpu_status_ie()  {
    return {0, 1};
  }
  static  FpuStatus fpu_status_de()  {
    return {1, 1};
  }
  static  FpuStatus fpu_status_ze()  {
    return {2, 1};
  }
  static  FpuStatus fpu_status_oe()  {
    return {3, 1};
  }
  static  FpuStatus fpu_status_ue()  {
    return {4, 1};
  }
  static  FpuStatus fpu_status_pe()  {
    return {5, 1};
  }
  static  FpuStatus fpu_status_sf()  {
    return {6, 1};
  }
  static  FpuStatus fpu_status_es()  {
    return {7, 1};
  }
  static  FpuStatus fpu_status_c0()  {
    return {8, 1};
  }
  static  FpuStatus fpu_status_c1()  {
    return {9, 1};
  }
  static  FpuStatus fpu_status_c2()  {
    return {10, 1};
  }
  static  FpuStatus fpu_status_top() {
    return {11, 3};
  }
  static  FpuStatus fpu_status_c3()  {
    return {14, 1};
  }
  static  FpuStatus fpu_status_b()   {
    return {15, 1};
  }
  static  std::array<FpuStatus,14> fpu_status() {
    return {
      fpu_status_ie(), fpu_status_de(), fpu_status_ze(), fpu_status_oe(),
      fpu_status_ue(), fpu_status_pe(), fpu_status_sf(), fpu_status_es(),
      fpu_status_c0(), fpu_status_c1(), fpu_status_c2(), fpu_status_top(),
      fpu_status_c3(), fpu_status_b()
    };
  }

  static  FpuTag tag0() {
    return {0, 2};
  }
  static  FpuTag tag1() {
    return {2, 2};
  }
  static  FpuTag tag2() {
    return {4, 2};
  }
  static  FpuTag tag3() {
    return {6, 2};
  }
  static  FpuTag tag4() {
    return {8, 2};
  }
  static  FpuTag tag5() {
    return {10, 2};
  }
  static  FpuTag tag6() {
    return {12, 2};
  }
  static  FpuTag tag7() {
    return {14, 2};
  }
  static  std::array<FpuTag,8> fpu_tags() {
    return {
      tag0(), tag1(), tag2(), tag3(), tag4(), tag5(), tag6(), tag7()
    };
  }

  static  Mxcsr mxcsr_ie()  {
    return {0, 1};
  }
  static  Mxcsr mxcsr_de()  {
    return {1, 1};
  }
  static  Mxcsr mxcsr_ze()  {
    return {2, 1};
  }
  static  Mxcsr mxcsr_oe()  {
    return {3, 1};
  }
  static  Mxcsr mxcsr_ue()  {
    return {4, 1};
  }
  static  Mxcsr mxcsr_pe()  {
    return {5, 1};
  }
  static  Mxcsr mxcsr_daz() {
    return {6, 1};
  }
  static  Mxcsr mxcsr_im()  {
    return {7, 1};
  }
  static  Mxcsr mxcsr_dm()  {
    return {8, 1};
  }
  static  Mxcsr mxcsr_zm()  {
    return {9, 1};
  }
  static  Mxcsr mxcsr_om()  {
    return {10, 1};
  }
  static  Mxcsr mxcsr_um()  {
    return {11, 1};
  }
  static  Mxcsr mxcsr_pm()  {
    return {12, 1};
  }
  static  Mxcsr mxcsr_rc()  {
    return {13, 2};
  }
  static  Mxcsr mxcsr_fz()  {
    return {15, 1};
  }
  static  std::array<Mxcsr,15> mxcsr() {
    return {
      mxcsr_ie(), mxcsr_de(), mxcsr_ze(),  mxcsr_oe(),
      mxcsr_ue(), mxcsr_pe(), mxcsr_daz(), mxcsr_im(),
      mxcsr_dm(), mxcsr_zm(), mxcsr_om(),  mxcsr_um(),
      mxcsr_pm(), mxcsr_rc(), mxcsr_fz()
    };
  }

  static  Rip rip() {
    return {};
  }
  static  FpuData fpu_data() {
    return {};
  }
  static  FpuInstruction fpu_instruction() {
    return {};
  }
  static  FpuOpcode fpu_opcode() {
    return {};
  }

  static Hint taken() {
    return {0};
  }
  static Hint not_taken() {
    return {1};
  }

  static  Zero zero() {
    return {};
  }
  static  One one() {
    return {};
  }
  static  Three three() {
    return {};
  }

  static  Mm mm0() {
    return {0};
  }
  static  Mm mm1() {
    return {1};
  }
  static  Mm mm2() {
    return {2};
  }
  static  Mm mm3() {
    return {3};
  }
  static  Mm mm4() {
    return {4};
  }
  static  Mm mm5() {
    return {5};
  }
  static  Mm mm6() {
    return {6};
  }
  static  Mm mm7() {
    return {7};
  }
  static  std::array<Mm,8> mms() {
    return {
      mm0(), mm1(), mm2(), mm3(), mm4(), mm5(), mm6(), mm7()
    };
  }

  static  Pref66 pref_66() {
    return {};
  }
  static  PrefRexW pref_rex_w() {
    return {};
  }
  static  Far far() {
    return {};
  }

  static  Al al() {
    return {};
  }
  static  Cl cl() {
    return {};
  }
  static  R8 dl() {
    return {2};
  }
  static  R8 bl() {
    return {3};
  }
  static  R8 spl()  {
    return {4};
  }
  static  R8 bpl()  {
    return {5};
  }
  static  R8 sil()  {
    return {6};
  }
  static  R8 dil()  {
    return {7};
  }
  static  R8 r8b()  {
    return {8};
  }
  static  R8 r9b()  {
    return {9};
  }
  static  R8 r10b() {
    return {10};
  }
  static  R8 r11b() {
    return {11};
  }
  static  R8 r12b() {
    return {12};
  }
  static  R8 r13b() {
    return {13};
  }
  static  R8 r14b() {
    return {14};
  }
  static  R8 r15b() {
    return {15};
  }
  static  std::array<R8, 16> r8s() {
    return {
      al(), cl(), dl(), bl(),
      spl(), bpl(), sil(), dil(), 
      r8b(), r9b(), r10b(), r11b(), 
      r12b(), r13b(), r14b(), r15b()
    };
  }

  static  Rh ah() {
    return {4};
  }
  static  Rh ch() {
    return {5};
  }
  static  Rh dh() {
    return {6};
  }
  static  Rh bh() {
    return {7};
  }
  static  std::array<Rh,4> rhs() {
    return {
      ah(), ch(), dh(), bh()
    };
  }

  static  Ax ax()    {
    return {};
  }
  static  R16 cx()   {
    return {1};
  }
  static  Dx dx()    {
    return {};
  }
  static  R16 bx()   {
    return {3};
  }
  static  R16 sp()   {
    return {4};
  }
  static  R16 bp()   {
    return {5};
  }
  static  R16 si()   {
    return {6};
  }
  static  R16 di()   {
    return {7};
  }
  static  R16 r8w()  {
    return {8};
  }
  static  R16 r9w()  {
    return {9};
  }
  static  R16 r10w() {
    return {10};
  }
  static  R16 r11w() {
    return {11};
  }
  static  R16 r12w() {
    return {12};
  }
  static  R16 r13w() {
    return {13};
  }
  static  R16 r14w() {
    return {14};
  }
  static  R16 r15w() {
    return {15};
  }
  static  std::array<R16, 16> r16s() {
    return {
      ax(),  cx(),  dx(),   bx(),   sp(),   bp(),   si(),   di(),
      r8w(), r9w(), r10w(), r11w(), r12w(), r13w(), r14w(), r15w()
    };
  }

  static  Eax eax()  {
    return {};
  }
  static  R32 ecx()  {
    return {1};
  }
  static  R32 edx()  {
    return {2};
  }
  static  R32 ebx()  {
    return {3};
  }
  static  R32 esp()  {
    return {4};
  }
  static  R32 ebp()  {
    return {5};
  }
  static  R32 esi()  {
    return {6};
  }
  static  R32 edi()  {
    return {7};
  }
  static  R32 r8d()  {
    return {8};
  }
  static  R32 r9d()  {
    return {9};
  }
  static  R32 r10d() {
    return {10};
  }
  static  R32 r11d() {
    return {11};
  }
  static  R32 r12d() {
    return {12};
  }
  static  R32 r13d() {
    return {13};
  }
  static  R32 r14d() {
    return {14};
  }
  static  R32 r15d() {
    return {15};
  }
  static  std::array<R32,16> r32s() {
    return {
      eax(), ecx(), edx(),  ebx(),  esp(),  ebp(),  esi(),  edi(),
      r8d(), r9d(), r10d(), r11d(), r12d(), r13d(), r14d(), r15d()
    };
  }

  static  Rax rax() {
    return {};
  }
  static  R64 rcx() {
    return {1};
  }
  static  R64 rdx() {
    return {2};
  }
  static  R64 rbx() {
    return {3};
  }
  static  R64 rsp() {
    return {4};
  }
  static  R64 rbp() {
    return {5};
  }
  static  R64 rsi() {
    return {6};
  }
  static  R64 rdi() {
    return {7};
  }
  static  R64 r8()  {
    return {8};
  }
  static  R64 r9()  {
    return {9};
  }
  static  R64 r10() {
    return {10};
  }
  static  R64 r11() {
    return {11};
  }
  static  R64 r12() {
    return {12};
  }
  static  R64 r13() {
    return {13};
  }
  static  R64 r14() {
    return {14};
  }
  static  R64 r15() {
    return {15};
  }
  static  std::array<R64, 16> r64s() {
    return {
      rax(), rcx(), rdx(), rbx(), rsp(), rbp(), rsi(), rdi(),
      r8(),  r9(),  r10(), r11(), r12(), r13(), r14(), r15()
    };
  }

  static  Sreg es() {
    return {0};
  }
  static  Sreg cs() {
    return {1};
  }
  static  Sreg ss() {
    return {2};
  }
  static  Sreg ds() {
    return {3};
  }
  static  Fs fs()   {
    return {};
  }
  static  Gs gs()   {
    return {};
  }
  static  std::array<Sreg, 6> sregs() {
    return {
      es(), cs(), ss(), ds(), fs(), gs()
    };
  }

  static  St0 st0() {
    return {};
  }
  static  St st1()  {
    return {1};
  }
  static  St st2()  {
    return {2};
  }
  static  St st3()  {
    return {3};
  }
  static  St st4()  {
    return {4};
  }
  static  St st5()  {
    return {5};
  }
  static  St st6()  {
    return {6};
  }
  static  St st7()  {
    return {7};
  }
  static  std::array<St,8> sts() {
    return {
      st0(), st1(), st2(), st3(), st4(), st5(), st6(), st7()
    };
  }

  static  Xmm0 xmm0()  {
    return {};
  }
  static  Xmm xmm1()   {
    return {1};
  }
  static  Xmm xmm2()   {
    return {2};
  }
  static  Xmm xmm3()   {
    return {3};
  }
  static  Xmm xmm4()   {
    return {4};
  }
  static  Xmm xmm5()   {
    return {5};
  }
  static  Xmm xmm6()   {
    return {6};
  }
  static  Xmm xmm7()   {
    return {7};
  }
  static  Xmm xmm8()   {
    return {8};
  }
  static  Xmm xmm9()   {
    return {9};
  }
  static  Xmm xmm10()  {
    return {10};
  }
  static  Xmm xmm11()  {
    return {11};
  }
  static  Xmm xmm12()  {
    return {12};
  }
  static  Xmm xmm13()  {
    return {13};
  }
  static  Xmm xmm14()  {
    return {14};
  }
  static  Xmm xmm15()  {
    return {15};
  }
  static  std::array<Xmm,16> xmms() {
    return {
      xmm0(), xmm1(), xmm2(),  xmm3(),  xmm4(),  xmm5(),  xmm6(),  xmm7(),
      xmm8(), xmm9(), xmm10(), xmm11(), xmm12(), xmm13(), xmm14(), xmm15()
    };
  }

  static  Ymm ymm0()  {
    return {0};
  }
  static  Ymm ymm1()  {
    return {1};
  }
  static  Ymm ymm2()  {
    return {2};
  }
  static  Ymm ymm3()  {
    return {3};
  }
  static  Ymm ymm4()  {
    return {4};
  }
  static  Ymm ymm5()  {
    return {5};
  }
  static  Ymm ymm6()  {
    return {6};
  }
  static  Ymm ymm7()  {
    return {7};
  }
  static  Ymm ymm8()  {
    return {8};
  }
  static  Ymm ymm9()  {
    return {9};
  }
  static  Ymm ymm10() {
    return {10};
  }
  static  Ymm ymm11() {
    return {11};
  }
  static  Ymm ymm12() {
    return {12};
  }
  static  Ymm ymm13() {
    return {13};
  }
  static  Ymm ymm14() {
    return {14};
  }
  static  Ymm ymm15() {
    return {15};
  }
  static  std::array<Ymm,16> ymms() {
    return {
      ymm0(), ymm1(), ymm2(),  ymm3(),  ymm4(),  ymm5(),  ymm6(),  ymm7(),
      ymm8(), ymm9(), ymm10(), ymm11(), ymm12(), ymm13(), ymm14(), ymm15()
    };
  }
};

 auto eflags_cf = Constants::eflags_cf();
 auto eflags_res1 = Constants::eflags_res1();
 auto eflags_pf = Constants::eflags_pf();
 auto eflags_res3 = Constants::eflags_res3();
 auto eflags_af = Constants::eflags_af();
 auto eflags_res5 = Constants::eflags_res5();
 auto eflags_zf = Constants::eflags_zf();
 auto eflags_sf = Constants::eflags_sf();
 auto eflags_tf = Constants::eflags_tf();
 auto eflags_if = Constants::eflags_if();
 auto eflags_df = Constants::eflags_df();
 auto eflags_of = Constants::eflags_of();
 auto eflags_iopl = Constants::eflags_iopl();
 auto eflags_nt = Constants::eflags_nt();
 auto eflags_res15 = Constants::eflags_res15();
 auto eflags_rf = Constants::eflags_rf();
 auto eflags_vm = Constants::eflags_vm();
 auto eflags_ac = Constants::eflags_ac();
 auto eflags_vif = Constants::eflags_vif();
 auto eflags_vip = Constants::eflags_vip();
 auto eflags_id = Constants::eflags_id();
 auto eflags = Constants::eflags();

 auto fpu_control_im = Constants::fpu_control_im();
 auto fpu_control_dm = Constants::fpu_control_dm();
 auto fpu_control_zm = Constants::fpu_control_zm();
 auto fpu_control_om = Constants::fpu_control_om();
 auto fpu_control_um = Constants::fpu_control_um();
 auto fpu_control_pm = Constants::fpu_control_pm();
 auto fpu_control_res6 = Constants::fpu_control_res6();
 auto fpu_control_res7 = Constants::fpu_control_res7();
 auto fpu_control_pc = Constants::fpu_control_pc();
 auto fpu_control_rc = Constants::fpu_control_rc();
 auto fpu_control_x = Constants::fpu_control_x();
 auto fpu_control_res13 = Constants::fpu_control_res13();
 auto fpu_control_res14 = Constants::fpu_control_res14();
 auto fpu_control_res15 = Constants::fpu_control_res15();
 auto fpu_control = Constants::fpu_control();

 auto fpu_status_ie = Constants::fpu_status_ie();
 auto fpu_status_de = Constants::fpu_status_de();
 auto fpu_status_ze = Constants::fpu_status_ze();
 auto fpu_status_oe = Constants::fpu_status_oe();
 auto fpu_status_ue = Constants::fpu_status_ue();
 auto fpu_status_pe = Constants::fpu_status_pe();
 auto fpu_status_sf = Constants::fpu_status_sf();
 auto fpu_status_es = Constants::fpu_status_es();
 auto fpu_status_c0 = Constants::fpu_status_c0();
 auto fpu_status_c1 = Constants::fpu_status_c1();
 auto fpu_status_c2 = Constants::fpu_status_c2();
 auto fpu_status_top = Constants::fpu_status_top();
 auto fpu_status_c3 = Constants::fpu_status_c3();
 auto fpu_status_b = Constants::fpu_status_b();
 auto fpu_status = Constants::fpu_status();

 auto tag0 = Constants::tag0();
 auto tag1 = Constants::tag1();
 auto tag2 = Constants::tag2();
 auto tag3 = Constants::tag3();
 auto tag4 = Constants::tag4();
 auto tag5 = Constants::tag5();
 auto tag6 = Constants::tag6();
 auto tag7 = Constants::tag7();
 auto fpu_tags = Constants::fpu_tags();

 auto mxcsr_ie = Constants::mxcsr_ie();
 auto mxcsr_de = Constants::mxcsr_de();
 auto mxcsr_ze = Constants::mxcsr_ze();
 auto mxcsr_oe = Constants::mxcsr_oe();
 auto mxcsr_ue = Constants::mxcsr_ue();
 auto mxcsr_pe = Constants::mxcsr_pe();
 auto mxcsr_daz = Constants::mxcsr_daz();
 auto mxcsr_im = Constants::mxcsr_im();
 auto mxcsr_dm = Constants::mxcsr_dm();
 auto mxcsr_zm = Constants::mxcsr_zm();
 auto mxcsr_om = Constants::mxcsr_om();
 auto mxcsr_um = Constants::mxcsr_um();
 auto mxcsr_pm = Constants::mxcsr_pm();
 auto mxcsr_rc = Constants::mxcsr_rc();
 auto mxcsr_fz = Constants::mxcsr_fz();
 auto mxcsr = Constants::mxcsr();

 auto fpu_data = Constants::fpu_data();
 auto fpu_instruction = Constants::fpu_instruction();
 auto fpu_opcode = Constants::fpu_opcode();
 auto rip = Constants::rip();

 auto taken = Constants::taken();
 auto not_taken = Constants::not_taken();

 auto zero = Constants::zero();
 auto one = Constants::one();
 auto three = Constants::three();

 auto mm0 = Constants::mm0();
 auto mm1 = Constants::mm1();
 auto mm2 = Constants::mm2();
 auto mm3 = Constants::mm3();
 auto mm4 = Constants::mm4();
 auto mm5 = Constants::mm5();
 auto mm6 = Constants::mm6();
 auto mm7 = Constants::mm7();
 auto mms = Constants::mms();

 auto pref_66 = Constants::pref_66();
 auto pref_rex_w = Constants::pref_rex_w();
 auto far = Constants::far();

 auto al = Constants::al();
 auto cl = Constants::cl();
 auto dl = Constants::dl();
 auto bl = Constants::bl();
 auto spl = Constants::spl();
 auto bpl = Constants::bpl();
 auto sil = Constants::sil();
 auto dil = Constants::dil();
 auto r8b = Constants::r8b();
 auto r9b = Constants::r9b();
 auto r10b = Constants::r10b();
 auto r11b = Constants::r11b();
 auto r12b = Constants::r12b();
 auto r13b = Constants::r13b();
 auto r14b = Constants::r14b();
 auto r15b = Constants::r15b();
 auto r8s = Constants::r8s();

 auto ah = Constants::ah();
 auto ch = Constants::ch();
 auto dh = Constants::dh();
 auto bh = Constants::bh();
 auto rhs = Constants::rhs();

 auto ax = Constants::ax();
 auto cx = Constants::cx();
 auto dx = Constants::dx();
 auto bx = Constants::bx();
 auto sp = Constants::sp();
 auto bp = Constants::bp();
 auto si = Constants::si();
 auto di = Constants::di();
 auto r8w = Constants::r8w();
 auto r9w = Constants::r9w();
 auto r10w = Constants::r10w();
 auto r11w = Constants::r11w();
 auto r12w = Constants::r12w();
 auto r13w = Constants::r13w();
 auto r14w = Constants::r14w();
 auto r15w = Constants::r15w();
 auto r16s = Constants::r16s();

 auto eax = Constants::eax();
 auto ecx = Constants::ecx();
 auto edx = Constants::edx();
 auto ebx = Constants::ebx();
 auto esp = Constants::esp();
 auto ebp = Constants::ebp();
 auto esi = Constants::esi();
 auto edi = Constants::edi();
 auto r8d = Constants::r8d();
 auto r9d = Constants::r9d();
 auto r10d = Constants::r10d();
 auto r11d = Constants::r11d();
 auto r12d = Constants::r12d();
 auto r13d = Constants::r13d();
 auto r14d = Constants::r14d();
 auto r15d = Constants::r15d();
 auto r32s = Constants::r32s();

 auto rax = Constants::rax();
 auto rcx = Constants::rcx();
 auto rdx = Constants::rdx();
 auto rbx = Constants::rbx();
 auto rsp = Constants::rsp();
 auto rbp = Constants::rbp();
 auto rsi = Constants::rsi();
 auto rdi = Constants::rdi();
 auto r8 = Constants::r8();
 auto r9 = Constants::r9();
 auto r10 = Constants::r10();
 auto r11 = Constants::r11();
 auto r12 = Constants::r12();
 auto r13 = Constants::r13();
 auto r14 = Constants::r14();
 auto r15 = Constants::r15();
 auto r64s = Constants::r64s();

 auto es = Constants::es();
 auto cs = Constants::cs();
 auto ss = Constants::ss();
 auto ds = Constants::ds();
 auto fs = Constants::fs();
 auto gs = Constants::gs();
 auto sregs = Constants::sregs();

 auto st0 = Constants::st0();
 auto st1 = Constants::st1();
 auto st2 = Constants::st2();
 auto st3 = Constants::st3();
 auto st4 = Constants::st4();
 auto st5 = Constants::st5();
 auto st6 = Constants::st6();
 auto st7 = Constants::st7();
 auto sts = Constants::sts();

 auto xmm0 = Constants::xmm0();
 auto xmm1 = Constants::xmm1();
 auto xmm2 = Constants::xmm2();
 auto xmm3 = Constants::xmm3();
 auto xmm4 = Constants::xmm4();
 auto xmm5 = Constants::xmm5();
 auto xmm6 = Constants::xmm6();
 auto xmm7 = Constants::xmm7();
 auto xmm8 = Constants::xmm8();
 auto xmm9 = Constants::xmm9();
 auto xmm10 = Constants::xmm10();
 auto xmm11 = Constants::xmm11();
 auto xmm12 = Constants::xmm12();
 auto xmm13 = Constants::xmm13();
 auto xmm14 = Constants::xmm14();
 auto xmm15 = Constants::xmm15();
 auto xmms = Constants::xmms();

 auto ymm0 = Constants::ymm0();
 auto ymm1 = Constants::ymm1();
 auto ymm2 = Constants::ymm2();
 auto ymm3 = Constants::ymm3();
 auto ymm4 = Constants::ymm4();
 auto ymm5 = Constants::ymm5();
 auto ymm6 = Constants::ymm6();
 auto ymm7 = Constants::ymm7();
 auto ymm8 = Constants::ymm8();
 auto ymm9 = Constants::ymm9();
 auto ymm10 = Constants::ymm10();
 auto ymm11 = Constants::ymm11();
 auto ymm12 = Constants::ymm12();
 auto ymm13 = Constants::ymm13();
 auto ymm14 = Constants::ymm14();
 auto ymm15 = Constants::ymm15();
 auto ymms = Constants::ymms();

} // namespace x64asm

#endif
