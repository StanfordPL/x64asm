#include "src/code/constants.h"

using namespace std;

namespace x64 {

// Constant Registers (cr.h)
const Cr0234 cr0{0};
const Cr0234 cr2{2};
const Cr0234 cr3{3};
const Cr0234 cr4{4};
const Cr8 cr8{8};

// Debug Registers (dr.h)
const Dr dr0{0};
const Dr dr1{1};
const Dr dr2{2};
const Dr dr3{3};
const Dr dr4{4};
const Dr dr5{5};
const Dr dr6{6};
const Dr dr7{7};

// Eflags (eflag.h2})
const Eflag cf{0};
const Eflag pf{2};
const Eflag af{4};
const Eflag zf{6};
const Eflag sf{7};
const Eflag tf{8};
const Eflag if_{9};
const Eflag df{10};
const Eflag of{11};
const Eflag iopl0{12};
const Eflag iopl1{13};
const Eflag nt{14};
const Eflag rf{16};
const Eflag vm{17};
const Eflag ac{18};
const Eflag vif{19};
const Eflag vip{20};
const Eflag id{21};

// Immediates (imm.h)
const Zero zero{};
const One one{};
const Three three{};

// MMX Registers (mm.h)
const Mm mm0{0};
const Mm mm1{1};
const Mm mm2{2};
const Mm mm3{3};
const Mm mm4{4};
const Mm mm5{5};
const Mm mm6{6};
const Mm mm7{7};

// Modifiers (modifier.h)
const Pref66 pref_66{};
const PrefRexW pref_rex_w{};
const Far far{};

// General Purpose Registers (r.h)
const Rh ah{4};
const Rh ch{5};
const Rh dh{6};
const Rh bh{7};

const Al al{};
const Cl cl{};
const Rl dl{2};
const Rl bl{3};

const Rb spl{4};
const Rb bpl{5};
const Rb sil{6};
const Rb dil{7};
const Rb r8b{8};
const Rb r9b{9};
const Rb r10b{10};
const Rb r11b{11};
const Rb r12b{12};
const Rb r13b{13};
const Rb r14b{14};
const Rb r15b{15};

const Ax ax{};
const R16 cx{1};
const Dx dx{};
const R16 bx{3};
const R16 sp{4};
const R16 bp{5};
const R16 si{6};
const R16 di{7};
const R16 r8w{8};
const R16 r9w{9};
const R16 r10w{10};
const R16 r11w{11};
const R16 r12w{12};
const R16 r13w{13};
const R16 r14w{14};
const R16 r15w{15};

const Eax eax{};
const R32 ecx{1};
const R32 edx{2};
const R32 ebx{3};
const R32 esp{4};
const R32 ebp{5};
const R32 esi{6};
const R32 edi{7};
const R32 r8d{8};
const R32 r9d{9};
const R32 r10d{10};
const R32 r11d{11};
const R32 r12d{12};
const R32 r13d{13};
const R32 r14d{14};
const R32 r15d{15};

const Rax rax{};
const R64 rcx{1};
const R64 rdx{2};
const R64 rbx{3};
const R64 rsp{4};
const R64 rbp{5};
const R64 rsi{6};
const R64 rdi{7};
const R64 r8{8};
const R64 r9{9};
const R64 r10{10};
const R64 r11{11};
const R64 r12{12};
const R64 r13{13};
const R64 r14{14};
const R64 r15{15};

// Segment Registers (sreg.h)
const Sreg es{0};
const Sreg cs{1};
const Sreg ss{2};
const Sreg ds{3};
const Fs fs{};
const Gs gs{};

// Floating Point Registers (st.h)
const St0 st0{};
const St st1{1};
const St st2{2};
const St st3{3};
const St st4{4};
const St st5{5};
const St st6{6};
const St st7{7};

// Xmm Registers (xmm.h)
const Xmm0 xmm0{};
const Xmm xmm1{1};
const Xmm xmm2{2};
const Xmm xmm3{3};
const Xmm xmm4{4};
const Xmm xmm5{5};
const Xmm xmm6{6};
const Xmm xmm7{7};
const Xmm xmm8{8};
const Xmm xmm9{9};
const Xmm xmm10{10};
const Xmm xmm11{11};
const Xmm xmm12{12};
const Xmm xmm13{13};
const Xmm xmm14{14};
const Xmm xmm15{15};

// Ymm Registers (ymm.h)
const Ymm ymm0{0};
const Ymm ymm1{1};
const Ymm ymm2{2};
const Ymm ymm3{3};
const Ymm ymm4{4};
const Ymm ymm5{5};
const Ymm ymm6{6};
const Ymm ymm7{7};
const Ymm ymm8{8};
const Ymm ymm9{9};
const Ymm ymm10{10};
const Ymm ymm11{11};
const Ymm ymm12{12};
const Ymm ymm13{13};
const Ymm ymm14{14};
const Ymm ymm15{15};

} // namespace x64
