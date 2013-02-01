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
#include "src/imm.h"
#include "src/mm.h"
#include "src/modifier.h"
#include "src/r.h"
#include "src/sreg.h"
#include "src/st.h"
#include "src/xmm.h"
#include "src/ymm.h"

#include <vector>

namespace x64asm {

class Constants {
	public:
		static inline Eflags eflags_cf()   { return Eflags{0,1}; }
		static inline Eflags eflags_pf()   { return Eflags{2,1}; }
		static inline Eflags eflags_af()   { return Eflags{4,1}; }
		static inline Eflags eflags_zf()   { return Eflags{6,1}; }
		static inline Eflags eflags_sf()   { return Eflags{7,1}; }
		static inline Eflags eflags_tf()   { return Eflags{8,1}; }
		static inline Eflags eflags_if()   { return Eflags{9,1}; }
		static inline Eflags eflags_df()   { return Eflags{10,1}; }
		static inline Eflags eflags_of()   { return Eflags{11,1}; }
		static inline Eflags eflags_iopl() { return Eflags{13,1}; }
		static inline Eflags eflags_nt()   { return Eflags{14,1}; }
		static inline Eflags eflags_rf()   { return Eflags{16,1}; }
		static inline Eflags eflags_vm()   { return Eflags{17,1}; }
		static inline Eflags eflags_ac()   { return Eflags{18,1}; }
		static inline Eflags eflags_vif()  { return Eflags{19,1}; }
		static inline Eflags eflags_vip()  { return Eflags{20,1}; }
		static inline Eflags eflags_id()   { return Eflags{21,1}; }

		static inline FpuControl fpu_control_im() { return FpuControl{0,1}; }
		static inline FpuControl fpu_control_dm() { return FpuControl{1,1}; }
		static inline FpuControl fpu_control_zm() { return FpuControl{2,1}; }
		static inline FpuControl fpu_control_om() { return FpuControl{3,1}; }
		static inline FpuControl fpu_control_um() { return FpuControl{4,1}; }
		static inline FpuControl fpu_control_pm() { return FpuControl{5,1}; }
		static inline FpuControl fpu_control_pc() { return FpuControl{9,2}; }
		static inline FpuControl fpu_control_rc() { return FpuControl{11,2}; }
		static inline FpuControl fpu_control_x()  { return FpuControl{12,1}; }

		static inline FpuStatus fpu_status_ie()  { return FpuStatus{0,1}; }
		static inline FpuStatus fpu_status_de()  { return FpuStatus{1,1}; }
		static inline FpuStatus fpu_status_ze()  { return FpuStatus{2,1}; }
		static inline FpuStatus fpu_status_oe()  { return FpuStatus{3,1}; }
		static inline FpuStatus fpu_status_ue()  { return FpuStatus{4,1}; }
		static inline FpuStatus fpu_status_pe()  { return FpuStatus{5,1}; }
		static inline FpuStatus fpu_status_sf()  { return FpuStatus{6,1}; }
		static inline FpuStatus fpu_status_es()  { return FpuStatus{7,1}; }
		static inline FpuStatus fpu_status_c0()  { return FpuStatus{8,1}; }
		static inline FpuStatus fpu_status_c1()  { return FpuStatus{9,1}; }
		static inline FpuStatus fpu_status_c2()  { return FpuStatus{10,1}; }
		static inline FpuStatus fpu_status_top() { return FpuStatus{13,3}; }
		static inline FpuStatus fpu_status_c3()  { return FpuStatus{14,1}; }
		static inline FpuStatus fpu_status_b()   { return FpuStatus{15,1}; }

		static inline FpuTag tag0() { return FpuTag{1,2}; }
		static inline FpuTag tag1() { return FpuTag{3,2}; }
		static inline FpuTag tag2() { return FpuTag{5,2}; }
		static inline FpuTag tag3() { return FpuTag{7,2}; }
		static inline FpuTag tag4() { return FpuTag{9,2}; }
		static inline FpuTag tag5() { return FpuTag{11,2}; }
		static inline FpuTag tag6() { return FpuTag{13,2}; }
		static inline FpuTag tag7() { return FpuTag{15,2}; }

		static inline Mxcsr mxcsr_ie()  { return Mxcsr{0,1}; }
		static inline Mxcsr mxcsr_de()  { return Mxcsr{1,1}; }
		static inline Mxcsr mxcsr_ze()  { return Mxcsr{2,1}; }
		static inline Mxcsr mxcsr_oe()  { return Mxcsr{3,1}; }
		static inline Mxcsr mxcsr_ue()  { return Mxcsr{4,1}; }
		static inline Mxcsr mxcsr_pe()  { return Mxcsr{5,1}; }
		static inline Mxcsr mxcsr_daz() { return Mxcsr{6,1}; }
		static inline Mxcsr mxcsr_im()  { return Mxcsr{7,1}; }
		static inline Mxcsr mxcsr_dm()  { return Mxcsr{8,1}; }
		static inline Mxcsr mxcsr_zm()  { return Mxcsr{9,1}; }
		static inline Mxcsr mxcsr_om()  { return Mxcsr{10,1}; }
		static inline Mxcsr mxcsr_um()  { return Mxcsr{11,1}; }
		static inline Mxcsr mxcsr_pm()  { return Mxcsr{12,1}; }
		static inline Mxcsr mxcsr_rc()  { return Mxcsr{14,2}; }
		static inline Mxcsr mxcsr_fz()  { return Mxcsr{15,1}; }

		static inline Rip rip() { return Rip{}; }
		static inline FpuData fpu_data() { return FpuData{}; }
		static inline FpuInstruction fpu_instruction() { return FpuInstruction{}; }
		static inline FpuOpcode fpu_opcode() { return FpuOpcode{}; }

		static inline Zero zero() { return Zero{}; }
		static inline One one() { return One{}; }
		static inline Three three() { return Three{}; }

		static inline Mm mm0() { return Mm{0}; }
		static inline Mm mm1() { return Mm{1}; }
		static inline Mm mm2() { return Mm{2}; }
		static inline Mm mm3() { return Mm{3}; }
		static inline Mm mm4() { return Mm{4}; }
		static inline Mm mm5() { return Mm{5}; }
		static inline Mm mm6() { return Mm{6}; }
		static inline Mm mm7() { return Mm{7}; }

		static inline Pref66 pref_66()      { return Pref66{}; }
		static inline PrefRexW pref_rex_w() { return PrefRexW{}; }
		static inline Far far()             { return Far{}; }

		static inline Al al() { return Al{}; }
		static inline Cl cl() { return Cl{}; }
		static inline Rl dl() { return Rl{2}; }
		static inline Rl bl() { return Rl{3}; }

		static inline Rh ah() { return Rh{4}; }
		static inline Rh ch() { return Rh{5}; }
		static inline Rh dh() { return Rh{6}; }
		static inline Rh bh() { return Rh{7}; }

		static inline Rb spl()  { return Rb{4}; }
		static inline Rb bpl()  { return Rb{5}; }
		static inline Rb sil()  { return Rb{6}; }
		static inline Rb dil()  { return Rb{7}; }
		static inline Rb r8b()  { return Rb{8}; }
		static inline Rb r9b()  { return Rb{9}; }
		static inline Rb r10b() { return Rb{10}; }
		static inline Rb r11b() { return Rb{11}; }
		static inline Rb r12b() { return Rb{12}; }
		static inline Rb r13b() { return Rb{13}; }
		static inline Rb r14b() { return Rb{14}; }
		static inline Rb r15b() { return Rb{15}; }
			
		static inline Ax ax()    { return Ax{}; }
		static inline R16 cx()   { return R16{1}; }
		static inline Dx dx()    { return Dx{}; }
		static inline R16 bx()   { return R16{3}; }
		static inline R16 sp()   { return R16{4}; }
		static inline R16 bp()   { return R16{5}; }
		static inline R16 si()   { return R16{6}; }
		static inline R16 di()   { return R16{7}; }
		static inline R16 r8w()  { return R16{8}; }
		static inline R16 r9w()  { return R16{9}; }
		static inline R16 r10w() { return R16{10}; }
		static inline R16 r11w() { return R16{11}; }
		static inline R16 r12w() { return R16{12}; }
		static inline R16 r13w() { return R16{13}; }
		static inline R16 r14w() { return R16{14}; }
		static inline R16 r15w() { return R16{15}; }

		static inline Eax eax()  { return Eax{}; }
		static inline R32 ecx()  { return R32{1}; }
		static inline R32 edx()  { return R32{2}; }
		static inline R32 ebx()  { return R32{3}; }
		static inline R32 esp()  { return R32{4}; }
		static inline R32 ebp()  { return R32{5}; }
		static inline R32 esi()  { return R32{6}; }
		static inline R32 edi()  { return R32{7}; }
		static inline R32 r8d()  { return R32{8}; }
		static inline R32 r9d()  { return R32{9}; }
		static inline R32 r10d() { return R32{10}; }
		static inline R32 r11d() { return R32{11}; }
		static inline R32 r12d() { return R32{12}; }
		static inline R32 r13d() { return R32{13}; }
		static inline R32 r14d() { return R32{14}; }
		static inline R32 r15d() { return R32{15}; }

		static inline Rax rax() { return Rax{}; }
		static inline R64 rcx() { return R64{1}; }
		static inline R64 rdx() { return R64{2}; }
		static inline R64 rbx() { return R64{3}; }
		static inline R64 rsp() { return R64{4}; }
		static inline R64 rbp() { return R64{5}; }
		static inline R64 rsi() { return R64{6}; }
		static inline R64 rdi() { return R64{7}; }
		static inline R64 r8()  { return R64{8}; }
		static inline R64 r9()  { return R64{9}; }
		static inline R64 r10() { return R64{10}; }
		static inline R64 r11() { return R64{11}; }
		static inline R64 r12() { return R64{12}; }
		static inline R64 r13() { return R64{13}; }
		static inline R64 r14() { return R64{14}; }
		static inline R64 r15() { return R64{15}; }

		static inline Sreg es() { return Sreg{0}; }
		static inline Sreg cs() { return Sreg{1}; }
		static inline Sreg ss() { return Sreg{2}; }
		static inline Sreg ds() { return Sreg{3}; }
		static inline Fs fs()   { return Fs{}; }
		static inline Gs gs()   { return Gs{}; }

		static inline St0 st0() { return St0{}; }
		static inline St st1()  { return St{1}; }
		static inline St st2()  { return St{2}; }
		static inline St st3()  { return St{3}; }
		static inline St st4()  { return St{4}; }
		static inline St st5()  { return St{5}; }
		static inline St st6()  { return St{6}; }
		static inline St st7()  { return St{7}; }

		static inline Xmm0 xmm0()  { return Xmm0{}; }
		static inline Xmm xmm1()   { return Xmm{1}; } 
		static inline Xmm xmm2()   { return Xmm{2}; } 
		static inline Xmm xmm3()   { return Xmm{3}; } 
		static inline Xmm xmm4()   { return Xmm{4}; } 
		static inline Xmm xmm5()   { return Xmm{5}; } 
		static inline Xmm xmm6()   { return Xmm{6}; } 
		static inline Xmm xmm7()   { return Xmm{7}; } 
		static inline Xmm xmm8()   { return Xmm{8}; } 
		static inline Xmm xmm9()   { return Xmm{9}; } 
		static inline Xmm xmm10()  { return Xmm{10}; } 
		static inline Xmm xmm11()  { return Xmm{11}; } 
		static inline Xmm xmm12()  { return Xmm{12}; } 
		static inline Xmm xmm13()  { return Xmm{13}; } 
		static inline Xmm xmm14()  { return Xmm{14}; } 
		static inline Xmm xmm15()  { return Xmm{15}; } 

		static inline Ymm ymm0()  { return Ymm{0}; }
		static inline Ymm ymm1()  { return Ymm{1}; } 
		static inline Ymm ymm2()  { return Ymm{2}; } 
		static inline Ymm ymm3()  { return Ymm{3}; } 
		static inline Ymm ymm4()  { return Ymm{4}; } 
		static inline Ymm ymm5()  { return Ymm{5}; } 
		static inline Ymm ymm6()  { return Ymm{6}; } 
		static inline Ymm ymm7()  { return Ymm{7}; } 
		static inline Ymm ymm8()  { return Ymm{8}; } 
		static inline Ymm ymm9()  { return Ymm{9}; } 
		static inline Ymm ymm10() { return Ymm{10}; } 
		static inline Ymm ymm11() { return Ymm{11}; } 
		static inline Ymm ymm12() { return Ymm{12}; } 
		static inline Ymm ymm13() { return Ymm{13}; } 
		static inline Ymm ymm14() { return Ymm{14}; } 
		static inline Ymm ymm15() { return Ymm{15}; } 
};

extern const FpuControl fpu_control_im;
extern const FpuControl fpu_control_dm;
extern const FpuControl fpu_control_zm;
extern const FpuControl fpu_control_om;
extern const FpuControl fpu_control_um;
extern const FpuControl fpu_control_pm;
extern const FpuControl fpu_control_pc;
extern const FpuControl fpu_control_rc;
extern const FpuControl fpu_control_x;

extern const std::vector<FpuControl> fpu_control;

extern const Eflags eflags_cf;
extern const Eflags eflags_pf;
extern const Eflags eflags_af;
extern const Eflags eflags_zf;
extern const Eflags eflags_sf;
extern const Eflags eflags_tf;
extern const Eflags eflags_if;
extern const Eflags eflags_df;
extern const Eflags eflags_of;
extern const Eflags eflags_iopl;
extern const Eflags eflags_nt;
extern const Eflags eflags_rf;
extern const Eflags eflags_vm;
extern const Eflags eflags_ac;
extern const Eflags eflags_vif;
extern const Eflags eflags_vip;
extern const Eflags eflags_id;

extern const std::vector<Eflags> eflags;

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

extern const std::vector<Mxcsr> mxcsr;

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

extern const std::vector<FpuStatus> fpu_status;

extern const FpuTag tag0;
extern const FpuTag tag1;
extern const FpuTag tag2;
extern const FpuTag tag3;
extern const FpuTag tag4;
extern const FpuTag tag5;
extern const FpuTag tag6;
extern const FpuTag tag7;

extern const std::vector<FpuTag> fpu_tags;

extern const FpuData fpu_data;
extern const FpuInstruction fpu_instruction;
extern const FpuOpcode fpu_opcode;
extern const Rip rip;

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

extern const std::vector<Mm> mms;

extern const Pref66 pref_66;
extern const PrefRexW pref_rexw;
extern const Far far;

extern const Al al;
extern const Cl cl;
extern const Rl dl;
extern const Rl bl;

extern const std::vector<Rl> rls;

extern const Rh ah;
extern const Rh ch;
extern const Rh dh;
extern const Rh bh;

extern const std::vector<Rh> rhs;

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

extern const std::vector<Rb> rbs;

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

extern const std::vector<R16> r16s;

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

extern const std::vector<R32> r32s;

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

extern const std::vector<R64> r64s;

extern const Sreg es;
extern const Sreg cs;
extern const Sreg ss;
extern const Sreg ds;
extern const Fs fs;
extern const Gs gs;

extern const std::vector<Sreg> sregs;

extern const St0 st0;
extern const St st1;
extern const St st2;
extern const St st3;
extern const St st4;
extern const St st5;
extern const St st6;
extern const St st7;

extern const std::vector<St> sts;

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

extern const std::vector<Xmm> xmms;

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

extern const std::vector<Ymm> ymms;

} // namespace x64asm

#endif
