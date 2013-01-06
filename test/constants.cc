#include <iostream>
#include <vector>

#include "include/x64.h"

using namespace x64;
using namespace std;

void foo() {
	cout << "Hello world!  You called this from assembly!" << endl;
}

template <typename T>
void all(const vector<T>& ts) {
	for ( const auto t : ts )
		cout << set_io(IO::ATT) << t << " ";
	cout << endl;
}

int main() {
	all(vector<Cr0234>{{cr0,cr2,cr3,cr4}});
	all(vector<Cr8>{{cr8}});
	all(vector<Dr>{{dr0,dr1,dr2,dr3,dr4,dr5,dr6,dr7}});
	all(vector<Eflag>{{cf,pf,af,zf,sf,tf,if_,df,of,iopl0,iopl1,nt,rf,vm,ac,vif,vip,id}});
	all(vector<Mm>{{mm0,mm1,mm2,mm3,mm4,mm5,mm6,mm7}});
	all(vector<Imm8>{{zero,one,three}});
	all(vector<NoRexR8>{{al,cl,dl,bl,ah,ch,dh,bh}});
	all(vector<RexR8>{{al,cl,dl,bl,spl,bpl,sil,dil,r8b,r9b,r10b,r11b,r12b,r13b,r14b,r15b}});
	all(vector<Rh>{{ah,ch,dh,bh}});
	all(vector<Rl>{{al,cl,dl,bl}});
	all(vector<Rb>{{spl,bpl,sil,dil,r8b,r9b,r10b,r11b,r12b,r13b,r14b,r15b}});
	all(vector<R16>{{ax,cx,dx,bx,sp,bp,si,di,r8w,r9w,r10w,r11w,r12w,r13w,r14w,r15w}});
	all(vector<R32>{{eax,ecx,edx,ebx,esp,ebp,esi,edi,r8d,r9d,r10d,r11d,r12d,r13d,r14d,r15d}});
	all(vector<R64>{{rax,rcx,rdx,rbx,rsp,rbp,rsi,rdi,r8,r9,r10,r11,r12,r13,r14,r15}});
	all(vector<Sreg>{{es,cs,ss,ds,fs,gs}});
	all(vector<St>{{st0,st1,st2,st3,st4,st5,st6,st7}});
	all(vector<Xmm>{{xmm0,xmm1,xmm2,xmm3,xmm4,xmm5,xmm6,xmm7,xmm8,xmm9,xmm10,xmm11,xmm12,xmm13,xmm14,xmm15}});
	all(vector<Ymm>{{ymm0,ymm1,ymm2,ymm3,ymm4,ymm5,ymm6,ymm7,ymm8,ymm9,ymm10,ymm11,ymm12,ymm13,ymm14,ymm15}});

	cout << "NOW THIS!" << endl;
	AttWriter::write(cout, rax);
	cout << endl;
	M128 m(rax, rdi, Scale::TIMES_2);
	cout << m << endl;
	cout << hex << m.val_ << endl;
	cout << endl;

	Assembler assm;
	Function f;

	assm.start(f);
	assm.ret();
	assm.ret();
	assm.ret();
	assm.ret();
	assm.ret();
	assm.ret();
	assm.ret();
	assm.ret();
	assm.ret();
	assm.ret();

	assm.finish();

	Function f2 = f;
	f2();

	cout << OpSet::empty() << endl;
	cout << (OpSet::universe()+=rbx) << endl;

	OpSet o = OpSet::empty();
	o += ah;
	o += al;
	o += rax;
	o += sp;
	o += r15d;
	o += rbp;
	cout << o << endl;

	return 0;
}
