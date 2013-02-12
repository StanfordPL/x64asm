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

%{

#include <map>
#include <sstream>
#include <string>

#include "src/constants.h"
#include "src/env_reg.h"
#include "src/imm.h"
#include "src/label.h"
#include "src/opcode.h"
#include "src/type.h"
#include "src/att.tab.h"

using namespace std;
using namespace x64asm;

int yy_line_number = 1;
static int colnum = 0;
#define YY_USER_ACTION {        \
	yylloc.first_line = yylineno; \
	yylloc.first_column = colnum; \
	colnum += yyleng;             \
	yylloc.last_column = colnum;  \
	yylloc.last_line = yylineno;  \
}

const Imm* to_imm(const char* c, bool is_hex, bool is_neg) {
	uint64_t val = 0;

	istringstream iss(c);
	if ( is_hex ) 
		iss >> hex >> val;
	else
		iss >> dec >> val;

	return is_neg ? new Imm8{-val} : new Imm8{val};
}

const Label* to_label(const char* c) {
	return new Label{Label::get(c)};
}

const std::string* to_str(const char* c, size_t len) {
	return new std::string(c, len);
}

%}

%option noyywrap nounput
%option yylineno

%%

","           { return COMMA; }
":"           { return COLON; }
"("           { return OPEN; }
")"           { return CLOSE; }
"\n"          { yy_line_number++; return ENDL; }
[ \t]         { ; }
"#"[^\n]*"\n" { yy_line_number++; return ENDL; }

"1" { yylval.scale = Scale::TIMES_1; return SCALE; }
"2" { yylval.scale = Scale::TIMES_2; return SCALE; }
"4" { yylval.scale = Scale::TIMES_4; return SCALE; }
"8" { yylval.scale = Scale::TIMES_8; return SCALE; }

"%rip" { yylval.rip = new Rip{rip}; return RIP; }

"<taken>"     { yylval.operand = new Hint{taken}; return HINT; }
"<not taken>" { yylval.operand = new Hint{not_taken}; return HINT; }

"-0x"[0-9a-f]+ { yylval.operand = to_imm(yytext+1, true, true); return OFFSET; }
"0x"[0-9a-f]+  { yylval.operand = to_imm(yytext, true, false);  return OFFSET; }

"$-0x"[0-9a-f]+ { yylval.operand = to_imm(yytext+2, true, true);  return IMM; }
"$0x"[0-9a-f]+  { yylval.operand = to_imm(yytext+1, true, false); return IMM; }

"."[a-z][a-zA-Z0-9]+ { yylval.operand = to_label(yytext); return LABEL; }

"<66>"   { yylval.operand = new Modifier{pref_66}; return PREF_66; }
"<rexw>" { yylval.operand = new Modifier{pref_rex_w}; return PREF_REX_W; }
"<far>"  { yylval.operand = new Modifier{far}; return FAR; }

"%mm0" { yylval.operand = new Mm{mm0}; return MM; }
"%mm1" { yylval.operand = new Mm{mm1}; return MM; }
"%mm2" { yylval.operand = new Mm{mm2}; return MM; }
"%mm3" { yylval.operand = new Mm{mm3}; return MM; }
"%mm4" { yylval.operand = new Mm{mm4}; return MM; }
"%mm5" { yylval.operand = new Mm{mm5}; return MM; }
"%mm6" { yylval.operand = new Mm{mm6}; return MM; }
"%mm7" { yylval.operand = new Mm{mm7}; return MM; }

"%al" { yylval.operand = new Al{al}; return AL; }
"%cl" { yylval.operand = new Cl{cl}; return CL; }
"%dl" { yylval.operand = new Rl{dl}; return RL; }
"%bl" { yylval.operand = new Rl{bl}; return RL; }

"%ah" { yylval.operand = new Rh{ah}; return RH; }
"%ch" { yylval.operand = new Rh{ch}; return RH; }
"%dh" { yylval.operand = new Rh{dh}; return RH; }
"%bh" { yylval.operand = new Rh{bh}; return RH; }
 
"%spl"  { yylval.operand = new Rb{spl};  return RB; }
"%bpl"  { yylval.operand = new Rb{bpl};  return RB; }
"%sil"  { yylval.operand = new Rb{sil};  return RB; }
"%dil"  { yylval.operand = new Rb{dil};  return RB; }
"%r8b"  { yylval.operand = new Rb{r8b};  return RB; }
"%r9b"  { yylval.operand = new Rb{r9b};  return RB; }
"%r10b" { yylval.operand = new Rb{r10b}; return RB; }
"%r11b" { yylval.operand = new Rb{r11b}; return RB; }
"%r12b" { yylval.operand = new Rb{r12b}; return RB; }
"%r13b" { yylval.operand = new Rb{r13b}; return RB; }
"%r14b" { yylval.operand = new Rb{r14b}; return RB; }
"%r15b" { yylval.operand = new Rb{r15b}; return RB; }

"%ax"   { yylval.operand = new Ax{ax};    return AX; }
"%cx"   { yylval.operand = new R16{cx};   return R_16; }
"%dx"   { yylval.operand = new Dx{dx};    return DX; }
"%bx"   { yylval.operand = new R16{bx};   return R_16; }
"%sp"   { yylval.operand = new R16{sp};   return R_16; }
"%bp"   { yylval.operand = new R16{bp};   return R_16; }
"%si"   { yylval.operand = new R16{si};   return R_16; }
"%di"   { yylval.operand = new R16{di};   return R_16; }
"%r8w"  { yylval.operand = new R16{r8w};  return R_16; }
"%r9w"  { yylval.operand = new R16{r9w};  return R_16; }
"%r10w" { yylval.operand = new R16{r10w}; return R_16; }
"%r11w" { yylval.operand = new R16{r11w}; return R_16; }
"%r12w" { yylval.operand = new R16{r12w}; return R_16; }
"%r13w" { yylval.operand = new R16{r13w}; return R_16; }
"%r14w" { yylval.operand = new R16{r14w}; return R_16; }
"%r15w" { yylval.operand = new R16{r15w}; return R_16; }

"%eax"  { yylval.operand = new Eax{eax}; return EAX; }
"%ecx"  { yylval.operand = new R32{ecx}; return R_32; }
"%edx"  { yylval.operand = new R32{edx}; return R_32; }
"%ebx"  { yylval.operand = new R32{ebx}; return R_32; }
"%esp"  { yylval.operand = new R32{esp}; return R_32; }
"%ebp"  { yylval.operand = new R32{ebp}; return R_32; }
"%esi"  { yylval.operand = new R32{esi}; return R_32; }
"%edi"  { yylval.operand = new R32{edi}; return R_32; }
"%r8d"  { yylval.operand = new R32{r8d};  return R_32; }
"%r9d"  { yylval.operand = new R32{r9d};  return R_32; }
"%r10d" { yylval.operand = new R32{r10d}; return R_32; }
"%r11d" { yylval.operand = new R32{r11d}; return R_32; }
"%r12d" { yylval.operand = new R32{r12d}; return R_32; }
"%r13d" { yylval.operand = new R32{r13d}; return R_32; }
"%r14d" { yylval.operand = new R32{r14d}; return R_32; }
"%r15d" { yylval.operand = new R32{r15d}; return R_32; }

"%rax" { yylval.operand = new Rax{rax}; return RAX; }
"%rcx" { yylval.operand = new R64{rcx}; return R_64; }
"%rdx" { yylval.operand = new R64{rdx}; return R_64; }
"%rbx" { yylval.operand = new R64{rbx}; return R_64; }
"%rsp" { yylval.operand = new R64{rsp}; return R_64; }
"%rbp" { yylval.operand = new R64{rbp}; return R_64; }
"%rsi" { yylval.operand = new R64{rsi}; return R_64; }
"%rdi" { yylval.operand = new R64{rdi}; return R_64; }
"%r8"  { yylval.operand = new R64{r8};  return R_64; }
"%r9"  { yylval.operand = new R64{r9};  return R_64; }
"%r10" { yylval.operand = new R64{r10}; return R_64; }
"%r11" { yylval.operand = new R64{r11}; return R_64; }
"%r12" { yylval.operand = new R64{r12}; return R_64; }
"%r13" { yylval.operand = new R64{r13}; return R_64; }
"%r14" { yylval.operand = new R64{r14}; return R_64; }
"%r15" { yylval.operand = new R64{r15}; return R_64; }

"%es" { yylval.operand = new Sreg{es}; return SREG; }
"%cs" { yylval.operand = new Sreg{cs}; return SREG; }
"%ss" { yylval.operand = new Sreg{ss}; return SREG; }
"%ds" { yylval.operand = new Sreg{ds}; return SREG; }
"%fs" { yylval.operand = new Fs{fs}; return FS; }
"%gs" { yylval.operand = new Gs{gs}; return GS; }

"%st"    { yylval.operand = new St0{st0}; return ST_0; }
"%st(0)" { yylval.operand = new St0{st0}; return ST_0; }
"%st(1)" { yylval.operand = new St{st1}; return ST; }
"%st(2)" { yylval.operand = new St{st2}; return ST; }
"%st(3)" { yylval.operand = new St{st3}; return ST; }
"%st(4)" { yylval.operand = new St{st4}; return ST; }
"%st(5)" { yylval.operand = new St{st5}; return ST; }
"%st(6)" { yylval.operand = new St{st6}; return ST; }
"%st(7)" { yylval.operand = new St{st7}; return ST; }

"%xmm0"  { yylval.operand = new Xmm0{xmm0};  return XMM_0; }
"%xmm1"  { yylval.operand = new Xmm{xmm1};  return XMM; }
"%xmm2"  { yylval.operand = new Xmm{xmm2};  return XMM; }
"%xmm3"  { yylval.operand = new Xmm{xmm3};  return XMM; }
"%xmm4"  { yylval.operand = new Xmm{xmm4};  return XMM; }
"%xmm5"  { yylval.operand = new Xmm{xmm5};  return XMM; }
"%xmm6"  { yylval.operand = new Xmm{xmm6};  return XMM; }
"%xmm7"  { yylval.operand = new Xmm{xmm7};  return XMM; }
"%xmm8"  { yylval.operand = new Xmm{xmm8};  return XMM; }
"%xmm9"  { yylval.operand = new Xmm{xmm9};  return XMM; }
"%xmm10" { yylval.operand = new Xmm{xmm10}; return XMM; }
"%xmm11" { yylval.operand = new Xmm{xmm11}; return XMM; }
"%xmm12" { yylval.operand = new Xmm{xmm12}; return XMM; }
"%xmm13" { yylval.operand = new Xmm{xmm13}; return XMM; }
"%xmm14" { yylval.operand = new Xmm{xmm14}; return XMM; }
"%xmm15" { yylval.operand = new Xmm{xmm15}; return XMM; }

"%ymm0"  { yylval.operand = new Ymm{ymm0};  return YMM; }
"%ymm1"  { yylval.operand = new Ymm{ymm1};  return YMM; }
"%ymm2"  { yylval.operand = new Ymm{ymm2};  return YMM; }
"%ymm3"  { yylval.operand = new Ymm{ymm3};  return YMM; }
"%ymm4"  { yylval.operand = new Ymm{ymm4};  return YMM; }
"%ymm5"  { yylval.operand = new Ymm{ymm5};  return YMM; }
"%ymm6"  { yylval.operand = new Ymm{ymm6};  return YMM; }
"%ymm7"  { yylval.operand = new Ymm{ymm7};  return YMM; }
"%ymm8"  { yylval.operand = new Ymm{ymm8};  return YMM; }
"%ymm9"  { yylval.operand = new Ymm{ymm9};  return YMM; }
"%ymm10" { yylval.operand = new Ymm{ymm10}; return YMM; }
"%ymm11" { yylval.operand = new Ymm{ymm11}; return YMM; }
"%ymm12" { yylval.operand = new Ymm{ymm12}; return YMM; }
"%ymm13" { yylval.operand = new Ymm{ymm13}; return YMM; }
"%ymm14" { yylval.operand = new Ymm{ymm14}; return YMM; }
"%ymm15" { yylval.operand = new Ymm{ymm15}; return YMM; }

[a-z]+" *"?    { yylval.opcode = to_str(yytext, yyleng); return OPCODE; }
"rep "[a-z]+   { yylval.opcode = to_str(yytext, yyleng); return OPCODE; }
"repe "[a-z]+  { yylval.opcode = to_str(yytext, yyleng); return OPCODE; }
"repne "[a-z]+ { yylval.opcode = to_str(yytext, yyleng); return OPCODE; }

. { yyterminate(); }

%%
