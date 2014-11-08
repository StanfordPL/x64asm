/*
Copyright 2013 eric schkufza

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

/** Predefined assembler constants. Direct access to the object constructors is
    disallowed in general. */
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

    static constexpr Rip rip() {
      return Rip();
    }
    static constexpr FpuData fpu_data() {
      return FpuData();
    }
    static constexpr FpuInstruction fpu_instruction() {
      return FpuInstruction();
    }
    static constexpr FpuOpcode fpu_opcode() {
      return FpuOpcode();
    }

    static constexpr Hint taken() {
      return {0};
    }
    static constexpr Hint not_taken() {
      return {1};
    }

    static constexpr Zero zero() {
      return Zero();
    }
    static constexpr One one() {
      return One();
    }
    static constexpr Three three() {
      return Three();
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

    static constexpr Pref66 pref_66() {
      return Pref66();
    }
    static constexpr PrefRexW pref_rex_w() {
      return PrefRexW();
    }
    static constexpr Far far() {
      return Far();
    }

    static constexpr Al al() {
      return Al();
    }
    static constexpr Cl cl() {
      return Cl();
    }
    static constexpr Rl dl() {
      return {2};
    }
    static constexpr Rl bl() {
      return {3};
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

    static constexpr Rb spl()  {
      return {4};
    }
    static constexpr Rb bpl()  {
      return {5};
    }
    static constexpr Rb sil()  {
      return {6};
    }
    static constexpr Rb dil()  {
      return {7};
    }
    static constexpr Rb r8b()  {
      return {8};
    }
    static constexpr Rb r9b()  {
      return {9};
    }
    static constexpr Rb r10b() {
      return {10};
    }
    static constexpr Rb r11b() {
      return {11};
    }
    static constexpr Rb r12b() {
      return {12};
    }
    static constexpr Rb r13b() {
      return {13};
    }
    static constexpr Rb r14b() {
      return {14};
    }
    static constexpr Rb r15b() {
      return {15};
    }

    static constexpr Ax ax()    {
      return Ax();
    }
    static constexpr R16 cx()   {
      return {1};
    }
    static constexpr Dx dx()    {
      return Dx();
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

    static constexpr Eax eax()  {
      return Eax();
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

    static constexpr Rax rax() {
      return Rax();
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
      return Fs();
    }
    static constexpr Gs gs()   {
      return Gs();
    }

    static constexpr St0 st0() {
      return St0();
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

    static constexpr Xmm0 xmm0()  {
      return Xmm0();
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
};

// Convenience definitions follow...

extern const FpuControl fpu_control_im;
extern const FpuControl fpu_control_dm;
extern const FpuControl fpu_control_zm;
extern const FpuControl fpu_control_om;
extern const FpuControl fpu_control_um;
extern const FpuControl fpu_control_pm;
extern const FpuControl fpu_control_res6;
extern const FpuControl fpu_control_res7;
extern const FpuControl fpu_control_pc;
extern const FpuControl fpu_control_rc;
extern const FpuControl fpu_control_x;
extern const FpuControl fpu_control_res13;
extern const FpuControl fpu_control_res14;
extern const FpuControl fpu_control_res15;

extern const std::array<FpuControl,14> fpu_control;

extern const Eflags eflags_cf;
extern const Eflags eflags_res1;
extern const Eflags eflags_pf;
extern const Eflags eflags_res3;
extern const Eflags eflags_af;
extern const Eflags eflags_res5;
extern const Eflags eflags_zf;
extern const Eflags eflags_sf;
extern const Eflags eflags_tf;
extern const Eflags eflags_if;
extern const Eflags eflags_df;
extern const Eflags eflags_of;
extern const Eflags eflags_iopl;
extern const Eflags eflags_nt;
extern const Eflags eflags_res15;
extern const Eflags eflags_rf;
extern const Eflags eflags_vm;
extern const Eflags eflags_ac;
extern const Eflags eflags_vif;
extern const Eflags eflags_vip;
extern const Eflags eflags_id;

extern const std::array<Eflags,21> eflags;

extern const Mxcsr mxcsr_ie;
extern const Mxcsr mxcsr_de;
extern const Mxcsr mxcsr_ze;
extern const Mxcsr mxcsr_oe;
extern const Mxcsr mxcsr_ue;
extern const Mxcsr mxcsr_pe;
extern const Mxcsr mxcsr_daz;
extern const Mxcsr mxcsr_im;
extern const Mxcsr mxcsr_dm;
extern const Mxcsr mxcsr_zm;
extern const Mxcsr mxcsr_om;
extern const Mxcsr mxcsr_um;
extern const Mxcsr mxcsr_pm;
extern const Mxcsr mxcsr_rc;
extern const Mxcsr mxcsr_fz;

extern const std::array<Mxcsr,15> mxcsr;

extern const FpuStatus fpu_status_ie;
extern const FpuStatus fpu_status_de;
extern const FpuStatus fpu_status_ze;
extern const FpuStatus fpu_status_oe;
extern const FpuStatus fpu_status_ue;
extern const FpuStatus fpu_status_pe;
extern const FpuStatus fpu_status_sf;
extern const FpuStatus fpu_status_es;
extern const FpuStatus fpu_status_c0;
extern const FpuStatus fpu_status_c1;
extern const FpuStatus fpu_status_c2;
extern const FpuStatus fpu_status_top;
extern const FpuStatus fpu_status_c3;
extern const FpuStatus fpu_status_b;

extern const std::array<FpuStatus,14> fpu_status;

extern const FpuTag tag0;
extern const FpuTag tag1;
extern const FpuTag tag2;
extern const FpuTag tag3;
extern const FpuTag tag4;
extern const FpuTag tag5;
extern const FpuTag tag6;
extern const FpuTag tag7;

extern const std::array<FpuTag,8> fpu_tags;

extern const FpuData fpu_data;
extern const FpuInstruction fpu_instruction;
extern const FpuOpcode fpu_opcode;
extern const Rip rip;

extern const Hint taken;
extern const Hint not_taken;

extern const Zero zero;
extern const One one;
extern const Three three;

extern const Mm mm0;
extern const Mm mm1;
extern const Mm mm2;
extern const Mm mm3;
extern const Mm mm4;
extern const Mm mm5;
extern const Mm mm6;
extern const Mm mm7;

extern const std::array<Mm,8> mms;

extern const Pref66 pref_66;
extern const PrefRexW pref_rex_w;
extern const Far far;

extern const Al al;
extern const Cl cl;
extern const Rl dl;
extern const Rl bl;

extern const std::array<Rl,4> rls;

extern const Rh ah;
extern const Rh ch;
extern const Rh dh;
extern const Rh bh;

extern const std::array<Rh,4> rhs;

extern const Rb spl;
extern const Rb bpl;
extern const Rb sil;
extern const Rb dil;
extern const Rb r8b;
extern const Rb r9b;
extern const Rb r10b;
extern const Rb r11b;
extern const Rb r12b;
extern const Rb r13b;
extern const Rb r14b;
extern const Rb r15b;

extern const std::array<Rb,12> rbs;

extern const Ax ax;
extern const R16 cx;
extern const Dx dx;
extern const R16 bx;
extern const R16 sp;
extern const R16 bp;
extern const R16 si;
extern const R16 di;
extern const R16 r8w;
extern const R16 r9w;
extern const R16 r10w;
extern const R16 r11w;
extern const R16 r12w;
extern const R16 r13w;
extern const R16 r14w;
extern const R16 r15w;

extern const std::array<R16,16> r16s;

extern const Eax eax;
extern const R32 ecx;
extern const R32 edx;
extern const R32 ebx;
extern const R32 esp;
extern const R32 ebp;
extern const R32 esi;
extern const R32 edi;
extern const R32 r8d;
extern const R32 r9d;
extern const R32 r10d;
extern const R32 r11d;
extern const R32 r12d;
extern const R32 r13d;
extern const R32 r14d;
extern const R32 r15d;

extern const std::array<R32,16> r32s;

extern const Rax rax;
extern const R64 rcx;
extern const R64 rdx;
extern const R64 rbx;
extern const R64 rsp;
extern const R64 rbp;
extern const R64 rsi;
extern const R64 rdi;
extern const R64 r8;
extern const R64 r9;
extern const R64 r10;
extern const R64 r11;
extern const R64 r12;
extern const R64 r13;
extern const R64 r14;
extern const R64 r15;

extern const std::array<R64,16> r64s;

extern const Sreg es;
extern const Sreg cs;
extern const Sreg ss;
extern const Sreg ds;
extern const Fs fs;
extern const Gs gs;

extern const std::array<Sreg,6> sregs;

extern const St0 st0;
extern const St st1;
extern const St st2;
extern const St st3;
extern const St st4;
extern const St st5;
extern const St st6;
extern const St st7;

extern const std::array<St,8> sts;

extern const Xmm0 xmm0;
extern const Xmm xmm1;
extern const Xmm xmm2;
extern const Xmm xmm3;
extern const Xmm xmm4;
extern const Xmm xmm5;
extern const Xmm xmm6;
extern const Xmm xmm7;
extern const Xmm xmm8;
extern const Xmm xmm9;
extern const Xmm xmm10;
extern const Xmm xmm11;
extern const Xmm xmm12;
extern const Xmm xmm13;
extern const Xmm xmm14;
extern const Xmm xmm15;

extern const std::array<Xmm,16> xmms;

extern const Ymm ymm0;
extern const Ymm ymm1;
extern const Ymm ymm2;
extern const Ymm ymm3;
extern const Ymm ymm4;
extern const Ymm ymm5;
extern const Ymm ymm6;
extern const Ymm ymm7;
extern const Ymm ymm8;
extern const Ymm ymm9;
extern const Ymm ymm10;
extern const Ymm ymm11;
extern const Ymm ymm12;
extern const Ymm ymm13;
extern const Ymm ymm14;
extern const Ymm ymm15;

extern const std::array<Ymm,16> ymms;

} // namespace x64asm

#endif
