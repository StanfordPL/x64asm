/*
Copyright 2013 eric schkufza

Licensed under the Apache License << " " <<  Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

#include <iostream>
#include <vector>

#include "include/x64asm.h"

using namespace std;
using namespace x64asm;

// This example demonstrates the set of built-in constants supported by x64asm.

int main() {
  // Constants are available individually
  cout << al    << " " << cl    << " " << dl    << " " << bl    << endl;
  cout << ah    << " " << ch    << " " << dh    << " " << bh    << endl;
  cout << spl   << " " << bpl   << " " << sil   << " " << dil   << " "
       << r8b   << " " << r9b   << " " << r10b  << " " << r11b  << " "
       << r12b  << " " << r13b  << " "
       << r14b  << " " << r15b  << endl;
  cout << ax    << " " << cx    << " " << dx    << " " << bx    << " "
       << sp    << " " << bp    << " " << si    << " " << di    << " "
       << r8w   << " " << r9w   << " " << r10w  << " " << r11w  << " "
       << r12w  << " " << r13w  << " " << r14w  << " " << r15w  << endl;
  cout << eax   << " " << ecx   << " " << edx   << " " << ebx   << " "
       << esp   << " " << ebp   << " " << esi   << " " << edi   << " "
       << r8d   << " " << r9d   << " " << r10d  << " " << r11d  << " "
       << r12d  << " " << r13d  << " " << r14d  << " " << r15d  << endl;
  cout << rax   << " " << rcx   << " " << rdx   << " " << rbx   << " "
       << rsp   << " " << rbp   << " " << rsi   << " " << rdi   << " "
       << r8    << " " << r9    << " " << r10   << " " << r11   << " "
       << r12   << " " << r13   << " " << r14   << " " << r15   << endl;
  cout << es    << " " << cs    << " " << ss    << " " << ds    << " "
       << fs    << " " << gs    << endl;
  cout << mm0   << " " << mm1   << " " << mm2   << " " << mm3   << " "
       << mm4   << " " << mm5   << " " << mm6   << " " << mm7   << endl;
  cout << st0   << " " << st1   << " " << st2   << " " << st3   << " "
       << st4   << " " << st5   << " " << st6   << " " << st7   << endl;
  cout << xmm0  << " " << xmm1  << " " << xmm2  << " " << xmm3  << " "
       << xmm4  << " " << xmm5  << " " << xmm6  << " " << xmm7  << " "
       << xmm8  << " " << xmm9  << " " << xmm10 << " " << xmm11 << " "
       << xmm12 << " " << xmm13 << " " << xmm14 << " " << xmm15 << endl;
  cout << ymm0  << " " << ymm1  << " " << ymm2  << " " << ymm3  << " "
       << ymm4  << " " << ymm5  << " " << ymm6  << " " << ymm7  << " "
       << ymm8  << " " << ymm9  << " " << ymm10 << " " << ymm11 << " "
       << ymm12 << " " << ymm13 << " " << ymm14 << " " << ymm15 << endl;

  cout << endl;

  for ( const auto& r : rhs )
    cout << r << " ";
  cout << endl;
  for ( const auto& r : r8s )
    cout << r << " ";
  cout << endl;
  for ( const auto& r : r16s )
    cout << r << " ";
  cout << endl;
  for ( const auto& r : r32s )
    cout << r << " ";
  cout << endl;
  for ( const auto& r : r64s )
    cout << r << " ";
  cout << endl;
  for ( const auto& s : sregs )
    cout << s << " ";
  cout << endl;
  for ( const auto& m : mms )
    cout << m << " ";
  cout << endl;
  for ( const auto& s : sts )
    cout << s << " ";
  cout << endl;
  for ( const auto& x : xmms )
    cout << x << " ";
  cout << endl;
  for ( const auto& y : ymms )
    cout << y << " ";
  cout << endl;
  cout << endl;

	// and also as part of reg sets
	cout << RegSet::empty() << endl;
	cout << RegSet::universe() << endl;
	cout << endl;

  // In addition to register constants, there are also predefined
  // special case immediates and instruction modifiers, which can be used as
  // assembler arguments to access special case instruction forms.
  cout << zero << " " << one << " " << three << endl;
  cout << pref_66 << " " << pref_rex_w << " " << far << endl;

  return 0;
}
