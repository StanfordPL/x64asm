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

#include "src/control.h"
#include "src/cr.h"
#include "src/dr.h"
#include "src/eflags.h"
#include "src/imm.h"
#include "src/mm.h"
#include "src/modifier.h"
#include "src/mxcsr.h"
#include "src/r.h"
#include "src/sreg.h"
#include "src/st.h"
#include "src/status.h"
#include "src/tag.h"
#include "src/xmm.h"
#include "src/ymm.h"

#include <vector>

namespace x64asm {

class Constants {
	public:
		static inline Control control_im() { return Control{0}; }
		static inline Control control_dm() { return Control{1}; }
		static inline Control control_zm() { return Control{2}; }
		static inline Control control_om() { return Control{3}; }
		static inline Control control_um() { return Control{4}; }
		static inline Control control_pm() { return Control{5}; }
		static inline Control control_pc() { return Control{9}; }
		static inline Control control_rc() { return Control{11}; }
		static inline Control control_x() { return Control{12}; }

		static inline Cr0234 cr0() { return Cr0234{0}; }
		static inline Cr0234 cr2() { return Cr0234{2}; }
		static inline Cr0234 cr3() { return Cr0234{3}; }
		static inline Cr0234 cr4() { return Cr0234{4}; }
		static inline Cr8 cr8() { return Cr8{}; }
		
		static inline Dr dr0() { return Dr{0}; }
		static inline Dr dr1() { return Dr{1}; }
		static inline Dr dr2() { return Dr{2}; }
		static inline Dr dr3() { return Dr{3}; }
		static inline Dr dr4() { return Dr{4}; }
		static inline Dr dr5() { return Dr{5}; }
		static inline Dr dr6() { return Dr{6}; }
		static inline Dr dr7() { return Dr{7}; }

		static inline Eflags eflags_cf()   { return Eflags{0}; }
		static inline Eflags eflags_pf()   { return Eflags{2}; }
		static inline Eflags eflags_af()   { return Eflags{4}; }
		static inline Eflags eflags_zf()   { return Eflags{6}; }
		static inline Eflags eflags_sf()   { return Eflags{7}; }
		static inline Eflags eflags_tf()   { return Eflags{8}; }
		static inline Eflags eflags_if_()  { return Eflags{9}; }
		static inline Eflags eflags_df()   { return Eflags{10}; }
		static inline Eflags eflags_of()   { return Eflags{11}; }
		static inline Eflags eflags_iopl() { return Eflags{13}; }
		static inline Eflags eflags_nt()   { return Eflags{14}; }
		static inline Eflags eflags_rf()   { return Eflags{16}; }
		static inline Eflags eflags_vm()   { return Eflags{17}; }
		static inline Eflags eflags_ac()   { return Eflags{18}; }
		static inline Eflags eflags_vif()  { return Eflags{19}; }
		static inline Eflags eflags_vip()  { return Eflags{20}; }
		static inline Eflags eflags_id()   { return Eflags{21}; }

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

		static inline Mxcsr mxcsr_ie()  { return Mxcsr{0}; }
		static inline Mxcsr mxcsr_de()  { return Mxcsr{1}; }
		static inline Mxcsr mxcsr_ze()  { return Mxcsr{2}; }
		static inline Mxcsr mxcsr_oe()  { return Mxcsr{3}; }
		static inline Mxcsr mxcsr_ue()  { return Mxcsr{4}; }
		static inline Mxcsr mxcsr_pe()  { return Mxcsr{5}; }
		static inline Mxcsr mxcsr_daz() { return Mxcsr{6}; }
		static inline Mxcsr mxcsr_im()  { return Mxcsr{7}; }
		static inline Mxcsr mxcsr_dm()  { return Mxcsr{8}; }
		static inline Mxcsr mxcsr_zm()  { return Mxcsr{9}; }
		static inline Mxcsr mxcsr_om()  { return Mxcsr{10}; }
		static inline Mxcsr mxcsr_um()  { return Mxcsr{11}; }
		static inline Mxcsr mxcsr_pm()  { return Mxcsr{12}; }
		static inline Mxcsr mxcsr_rc()  { return Mxcsr{14}; }
		static inline Mxcsr mxcsr_fz()  { return Mxcsr{15}; }

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

		static inline Status status_ie()  { return Status{0}; }
		static inline Status status_de()  { return Status{1}; }
		static inline Status status_ze()  { return Status{2}; }
		static inline Status status_oe()  { return Status{3}; }
		static inline Status status_ue()  { return Status{4}; }
		static inline Status status_pe()  { return Status{5}; }
		static inline Status status_sf()  { return Status{6}; }
		static inline Status status_es()  { return Status{7}; }
		static inline Status status_c0()  { return Status{8}; }
		static inline Status status_c1()  { return Status{9}; }
		static inline Status status_c2()  { return Status{10}; }
		static inline Status status_top() { return Status{13}; }
		static inline Status status_c3()  { return Status{14}; }
		static inline Status status_b()   { return Status{15}; }

		static inline Tag tag0() { return Tag{0}; }
		static inline Tag tag1() { return Tag{1}; }
		static inline Tag tag2() { return Tag{2}; }
		static inline Tag tag3() { return Tag{3}; }
		static inline Tag tag4() { return Tag{4}; }
		static inline Tag tag5() { return Tag{5}; }
		static inline Tag tag6() { return Tag{6}; }
		static inline Tag tag7() { return Tag{7}; }

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

namespace control {

extern const Control im;
extern const Control dm;
extern const Control zm;
extern const Control om;
extern const Control um;
extern const Control pm;
extern const Control pc;
extern const Control rc;
extern const Control x;

extern const std::vector<Control> control;

} // namespace control

namespace cr {

extern const Cr0234 cr0;
extern const Cr0234 cr2;
extern const Cr0234 cr3;
extern const Cr0234 cr4;
extern const Cr8 cr8;

extern const std::vector<Cr> crs;

} // namespace cr

namespace dr {

extern const Dr dr0;
extern const Dr dr1;
extern const Dr dr2;
extern const Dr dr3;
extern const Dr dr4;
extern const Dr dr5;
extern const Dr dr6;
extern const Dr dr7;

extern const std::vector<Dr> drs;

} // namespace dr

namespace eflags {

extern const Eflags cf;
extern const Eflags pf;
extern const Eflags af;
extern const Eflags zf;
extern const Eflags sf;
extern const Eflags tf;
extern const Eflags if_;
extern const Eflags df;
extern const Eflags of;
extern const Eflags iopl;
extern const Eflags nt;
extern const Eflags rf;
extern const Eflags vm;
extern const Eflags ac;
extern const Eflags vif;
extern const Eflags vip;
extern const Eflags id;

extern const std::vector<Eflags> eflags;

} // namespace eflags

namespace imm {

extern const Zero zero;
extern const One one;
extern const Three three;

} // namespace imm

namespace mm {

extern const Mm mm0;
extern const Mm mm1;
extern const Mm mm2;
extern const Mm mm3;
extern const Mm mm4;
extern const Mm mm5;
extern const Mm mm6;
extern const Mm mm7;

extern const std::vector<Mm> mms;

} // namespace mm

namespace mxcsr {

extern const Mxcsr ie;
extern const Mxcsr de;
extern const Mxcsr ze;
extern const Mxcsr oe;
extern const Mxcsr ue;
extern const Mxcsr pe;
extern const Mxcsr daz;
extern const Mxcsr im;
extern const Mxcsr dm;
extern const Mxcsr zm;
extern const Mxcsr om;
extern const Mxcsr um;
extern const Mxcsr pm;
extern const Mxcsr rc;
extern const Mxcsr fz;

extern const std::vector<Mxcsr> mxcsr;

} // namespace mxcsr

namespace modifier {

extern const Pref66 pref_66;
extern const PrefRexW pref_rexw;
extern const Far far;

} // namespace modifier

namespace rl {

extern const Al al;
extern const Cl cl;
extern const Rl dl;
extern const Rl bl;

extern const std::vector<Rl> rls;

} // namespace rl

namespace rh {

extern const Rh ah;
extern const Rh ch;
extern const Rh dh;
extern const Rh bh;

extern const std::vector<Rh> rhs;

} // namespace rh

namespace rb {

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

} // namespace rb

namespace r16 {

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

} // namespace r16

namespace r32 {

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

} // namespace r32

namespace r64 {

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

} // namespace r64

namespace sreg {

extern const Sreg es;
extern const Sreg cs;
extern const Sreg ss;
extern const Sreg ds;
extern const Fs fs;
extern const Gs gs;

extern const std::vector<Sreg> sregs;

} // namespace sreg

namespace st {

extern const St0 st0;
extern const St st1;
extern const St st2;
extern const St st3;
extern const St st4;
extern const St st5;
extern const St st6;
extern const St st7;

extern const std::vector<St> sts;

} // namespace st

namespace status {

extern const Status ie;
extern const Status de;
extern const Status ze;
extern const Status oe;
extern const Status ue;
extern const Status pe;
extern const Status sf;
extern const Status es;
extern const Status c0;
extern const Status c1;
extern const Status c2;
extern const Status top;
extern const Status c3;
extern const Status b;

extern const std::vector<Status> status;

} // namespace status

namespace tag {

extern const Tag tag0;
extern const Tag tag1;
extern const Tag tag2;
extern const Tag tag3;
extern const Tag tag4;
extern const Tag tag5;
extern const Tag tag6;
extern const Tag tag7;

extern const std::vector<Tag> tags;

} // namespace tag

namespace xmm {

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

} // namespace xmm

namespace ymm {

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

} // namespace

} // namespace x64asm

#endif
