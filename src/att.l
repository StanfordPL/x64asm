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

%{

 /*
Some high level notes as this implementation isn't totally obvious.

This lexer associates lexemes with the most general possible type
short of the universally applicable Operand type.  For example, %rax
is tagged as an R64 rather than an RAX.  

Immediates are assigned the generic type IMM (and stored in Imm64s).
The OFFSET type is used to identify numeric constants which are semantically
distinct from immediates.  These include memory offsets, moffs offsets, and
rel operands.  These as well, are stored in Imm64s. 

This lexing scheme allows for a simpler parser implementation by removing
the need to distinguish between general and special cases in parser rules.
 */

#include <map>
#include <sstream>
#include <string>
#include <cstring>

#include "src/constants.h"
#include "src/env_reg.h"
#include "src/imm.h"
#include "src/label.h"
#include "src/opcode.h"
#include "src/type.h"
#include "src/att.tab.h"

using namespace std;
using namespace x64asm;

#define YY_USER_ACTION \
	yylloc.first_line = yylloc.last_line = yylineno;

const Imm* to_imm(const char* c, bool is_hex, bool is_neg) {
	uint64_t val = 0;

	istringstream iss(c);
	if ( is_hex ) 
		iss >> hex >> val;
	else
		iss >> dec >> val;

	return is_neg ? new Imm64{-val} : new Imm64(val);
}

const std::string* to_str(const char* c, size_t len) {
	return new std::string(c, len);
}

const Label* to_label(const char* c) {
	return new Label(std::string(c));
}



%}

%option noyywrap nounput
%option yylineno

%%

","           { return COMMA; }
":"           { return COLON; }
"("           { return OPEN; }
")"           { return CLOSE; }
"\n"          { return ENDL; }
[ \t*]        { ; }
"#"[^\n]*"\n" { return ENDL; }
";"[^\n]*"\n" { return ENDL; }

"1" { yylval.scale = Scale::TIMES_1; return SCALE; }
"2" { yylval.scale = Scale::TIMES_2; return SCALE; }
"4" { yylval.scale = Scale::TIMES_4; return SCALE; }
"8" { yylval.scale = Scale::TIMES_8; return SCALE; }

"%rip" { yylval.rip = new Rip(Constants::rip()); return RIP; }

"<taken>"     { yylval.operand = new Hint(Constants::taken()); return HINT; }
"<not taken>" { yylval.operand = new Hint(Constants::not_taken()); return HINT; }

"-0x"[0-9a-fA-F]+ { yylval.operand = to_imm(yytext+1, true, true); return OFFSET; }
"0x"[0-9a-fA-F]+  { yylval.operand = to_imm(yytext, true, false);  return OFFSET; }

"$-0x"[0-9a-fA-F]+ { yylval.operand = to_imm(yytext+2, true, true);  return IMM; }
"$0x"[0-9a-fA-F]+  { yylval.operand = to_imm(yytext+1, true, false); return IMM; }

"."[a-zA-Z0-9_]+ { yylval.operand = to_label(yytext); return LABEL; }

"<66>"   { yylval.operand = new Modifier(Constants::pref_66()); return PREF_66; }
"<rexw>" { yylval.operand = new Modifier(Constants::pref_rex_w()); return PREF_REX_W; }
"<far>"  { yylval.operand = new Modifier(Constants::far()); return FAR; }

"%mm0" { yylval.operand = new Mm(Constants::mm0()); return MM; }
"%mm1" { yylval.operand = new Mm(Constants::mm1()); return MM; }
"%mm2" { yylval.operand = new Mm(Constants::mm2()); return MM; }
"%mm3" { yylval.operand = new Mm(Constants::mm3()); return MM; }
"%mm4" { yylval.operand = new Mm(Constants::mm4()); return MM; }
"%mm5" { yylval.operand = new Mm(Constants::mm5()); return MM; }
"%mm6" { yylval.operand = new Mm(Constants::mm6()); return MM; }
"%mm7" { yylval.operand = new Mm(Constants::mm7()); return MM; }

"%ah" { yylval.operand = new Rh(Constants::ah()); return RH; }
"%ch" { yylval.operand = new Rh(Constants::ch()); return RH; }
"%dh" { yylval.operand = new Rh(Constants::dh()); return RH; }
"%bh" { yylval.operand = new Rh(Constants::bh()); return RH; }
 
"%al"   { yylval.operand = new Al(Constants::al());   return R_8; }
"%cl"   { yylval.operand = new Cl(Constants::cl());   return R_8; }
"%dl"   { yylval.operand = new R8(Constants::dl());   return R_8; }
"%bl"   { yylval.operand = new R8(Constants::bl());   return R_8; }
"%spl"  { yylval.operand = new R8(Constants::spl());  return R_8; }
"%bpl"  { yylval.operand = new R8(Constants::bpl());  return R_8; }
"%sil"  { yylval.operand = new R8(Constants::sil());  return R_8; }
"%dil"  { yylval.operand = new R8(Constants::dil());  return R_8; }
"%r8b"  { yylval.operand = new R8(Constants::r8b());  return R_8; }
"%r9b"  { yylval.operand = new R8(Constants::r9b());  return R_8; }
"%r10b" { yylval.operand = new R8(Constants::r10b()); return R_8; }
"%r11b" { yylval.operand = new R8(Constants::r11b()); return R_8; }
"%r12b" { yylval.operand = new R8(Constants::r12b()); return R_8; }
"%r13b" { yylval.operand = new R8(Constants::r13b()); return R_8; }
"%r14b" { yylval.operand = new R8(Constants::r14b()); return R_8; }
"%r15b" { yylval.operand = new R8(Constants::r15b()); return R_8; }

"%ax"   { yylval.operand = new Ax(Constants::ax());    return R_16; }
"%cx"   { yylval.operand = new R16(Constants::cx());   return R_16; }
"%dx"   { yylval.operand = new Dx(Constants::dx());    return R_16; }
"%bx"   { yylval.operand = new R16(Constants::bx());   return R_16; }
"%sp"   { yylval.operand = new R16(Constants::sp());   return R_16; }
"%bp"   { yylval.operand = new R16(Constants::bp());   return R_16; }
"%si"   { yylval.operand = new R16(Constants::si());   return R_16; }
"%di"   { yylval.operand = new R16(Constants::di());   return R_16; }
"%r8w"  { yylval.operand = new R16(Constants::r8w());  return R_16; }
"%r9w"  { yylval.operand = new R16(Constants::r9w());  return R_16; }
"%r10w" { yylval.operand = new R16(Constants::r10w()); return R_16; }
"%r11w" { yylval.operand = new R16(Constants::r11w()); return R_16; }
"%r12w" { yylval.operand = new R16(Constants::r12w()); return R_16; }
"%r13w" { yylval.operand = new R16(Constants::r13w()); return R_16; }
"%r14w" { yylval.operand = new R16(Constants::r14w()); return R_16; }
"%r15w" { yylval.operand = new R16(Constants::r15w()); return R_16; }

"%eax"  { yylval.operand = new Eax(Constants::eax()); return R_32; }
"%ecx"  { yylval.operand = new R32(Constants::ecx()); return R_32; }
"%edx"  { yylval.operand = new R32(Constants::edx()); return R_32; }
"%ebx"  { yylval.operand = new R32(Constants::ebx()); return R_32; }
"%esp"  { yylval.operand = new R32(Constants::esp()); return R_32; }
"%ebp"  { yylval.operand = new R32(Constants::ebp()); return R_32; }
"%esi"  { yylval.operand = new R32(Constants::esi()); return R_32; }
"%edi"  { yylval.operand = new R32(Constants::edi()); return R_32; }
"%r8d"  { yylval.operand = new R32(Constants::r8d());  return R_32; }
"%r9d"  { yylval.operand = new R32(Constants::r9d());  return R_32; }
"%r10d" { yylval.operand = new R32(Constants::r10d()); return R_32; }
"%r11d" { yylval.operand = new R32(Constants::r11d()); return R_32; }
"%r12d" { yylval.operand = new R32(Constants::r12d()); return R_32; }
"%r13d" { yylval.operand = new R32(Constants::r13d()); return R_32; }
"%r14d" { yylval.operand = new R32(Constants::r14d()); return R_32; }
"%r15d" { yylval.operand = new R32(Constants::r15d()); return R_32; }

"%rax" { yylval.operand = new Rax(Constants::rax()); return R_64; }
"%rcx" { yylval.operand = new R64(Constants::rcx()); return R_64; }
"%rdx" { yylval.operand = new R64(Constants::rdx()); return R_64; }
"%rbx" { yylval.operand = new R64(Constants::rbx()); return R_64; }
"%rsp" { yylval.operand = new R64(Constants::rsp()); return R_64; }
"%rbp" { yylval.operand = new R64(Constants::rbp()); return R_64; }
"%rsi" { yylval.operand = new R64(Constants::rsi()); return R_64; }
"%rdi" { yylval.operand = new R64(Constants::rdi()); return R_64; }
"%r8"  { yylval.operand = new R64(Constants::r8());  return R_64; }
"%r9"  { yylval.operand = new R64(Constants::r9());  return R_64; }
"%r10" { yylval.operand = new R64(Constants::r10()); return R_64; }
"%r11" { yylval.operand = new R64(Constants::r11()); return R_64; }
"%r12" { yylval.operand = new R64(Constants::r12()); return R_64; }
"%r13" { yylval.operand = new R64(Constants::r13()); return R_64; }
"%r14" { yylval.operand = new R64(Constants::r14()); return R_64; }
"%r15" { yylval.operand = new R64(Constants::r15()); return R_64; }

"%es" { yylval.operand = new Sreg(Constants::es()); return SREG; }
"%cs" { yylval.operand = new Sreg(Constants::cs()); return SREG; }
"%ss" { yylval.operand = new Sreg(Constants::ss()); return SREG; }
"%ds" { yylval.operand = new Sreg(Constants::ds()); return SREG; }
"%fs" { yylval.operand = new Fs(Constants::fs()); return SREG; }
"%gs" { yylval.operand = new Gs(Constants::gs()); return SREG; }

"%st"    { yylval.operand = new St0(Constants::st0()); return ST; }
"%st(0)" { yylval.operand = new St0(Constants::st0()); return ST; }
"%st(1)" { yylval.operand = new St(Constants::st1()); return ST; }
"%st(2)" { yylval.operand = new St(Constants::st2()); return ST; }
"%st(3)" { yylval.operand = new St(Constants::st3()); return ST; }
"%st(4)" { yylval.operand = new St(Constants::st4()); return ST; }
"%st(5)" { yylval.operand = new St(Constants::st5()); return ST; }
"%st(6)" { yylval.operand = new St(Constants::st6()); return ST; }
"%st(7)" { yylval.operand = new St(Constants::st7()); return ST; }

"%xmm0"  { yylval.operand = new Xmm0(Constants::xmm0());  return XMM; }
"%xmm1"  { yylval.operand = new Xmm(Constants::xmm1());  return XMM; }
"%xmm2"  { yylval.operand = new Xmm(Constants::xmm2());  return XMM; }
"%xmm3"  { yylval.operand = new Xmm(Constants::xmm3());  return XMM; }
"%xmm4"  { yylval.operand = new Xmm(Constants::xmm4());  return XMM; }
"%xmm5"  { yylval.operand = new Xmm(Constants::xmm5());  return XMM; }
"%xmm6"  { yylval.operand = new Xmm(Constants::xmm6());  return XMM; }
"%xmm7"  { yylval.operand = new Xmm(Constants::xmm7());  return XMM; }
"%xmm8"  { yylval.operand = new Xmm(Constants::xmm8());  return XMM; }
"%xmm9"  { yylval.operand = new Xmm(Constants::xmm9());  return XMM; }
"%xmm10" { yylval.operand = new Xmm(Constants::xmm10()); return XMM; }
"%xmm11" { yylval.operand = new Xmm(Constants::xmm11()); return XMM; }
"%xmm12" { yylval.operand = new Xmm(Constants::xmm12()); return XMM; }
"%xmm13" { yylval.operand = new Xmm(Constants::xmm13()); return XMM; }
"%xmm14" { yylval.operand = new Xmm(Constants::xmm14()); return XMM; }
"%xmm15" { yylval.operand = new Xmm(Constants::xmm15()); return XMM; }

"%ymm0"  { yylval.operand = new Ymm(Constants::ymm0());  return YMM; }
"%ymm1"  { yylval.operand = new Ymm(Constants::ymm1());  return YMM; }
"%ymm2"  { yylval.operand = new Ymm(Constants::ymm2());  return YMM; }
"%ymm3"  { yylval.operand = new Ymm(Constants::ymm3());  return YMM; }
"%ymm4"  { yylval.operand = new Ymm(Constants::ymm4());  return YMM; }
"%ymm5"  { yylval.operand = new Ymm(Constants::ymm5());  return YMM; }
"%ymm6"  { yylval.operand = new Ymm(Constants::ymm6());  return YMM; }
"%ymm7"  { yylval.operand = new Ymm(Constants::ymm7());  return YMM; }
"%ymm8"  { yylval.operand = new Ymm(Constants::ymm8());  return YMM; }
"%ymm9"  { yylval.operand = new Ymm(Constants::ymm9());  return YMM; }
"%ymm10" { yylval.operand = new Ymm(Constants::ymm10()); return YMM; }
"%ymm11" { yylval.operand = new Ymm(Constants::ymm11()); return YMM; }
"%ymm12" { yylval.operand = new Ymm(Constants::ymm12()); return YMM; }
"%ymm13" { yylval.operand = new Ymm(Constants::ymm13()); return YMM; }
"%ymm14" { yylval.operand = new Ymm(Constants::ymm14()); return YMM; }
"%ymm15" { yylval.operand = new Ymm(Constants::ymm15()); return YMM; }

[a-z][a-z0-9]* { yylval.opcode = to_str(yytext, yyleng); return OPCODE; }
"rep "[a-z]+   { yylval.opcode = to_str(yytext, yyleng); return OPCODE; }
"repz "[a-z]+  { yylval.opcode = to_str(yytext, yyleng); return OPCODE; }
"repnz "[a-z]+ { yylval.opcode = to_str(yytext, yyleng); return OPCODE; }

. { yyterminate(); }

%%
