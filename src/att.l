%{

#include <map>
#include <sstream>

#include "src/constants.h"
#include "src/imm.h"
#include "src/label.h"
#include "src/opcode.h"
#include "src/op_type.h"
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

const Operand* to_label(const char* c) {
	static map<string, Label*> labels;
	if ( labels.find(c) == labels.end() ) {
		Label* l = new Label(labels.size());
		labels[c] = l;	
	}
	return labels[c];
}

const Operand* to_imm(const char* c, bool is_hex, bool is_neg) {
	istringstream iss(c);
	uint64_t val = 0;

	if ( is_hex ) 
		iss >> hex >> val;
	else
		iss >> dec >> val;

	int64_t ival = (int64_t)val;
	if ( ival >= -128 && ival < 128 )
		return new Imm8{(int8_t)ival};
	else if ( ival >= -32768 && ival < 32768 )
		return new Imm16{(int16_t)ival};
	else if ( ival >= -2147483648 && ival < 2147483648 )
		return new Imm32{(int32_t)ival};
	else
		return new Imm64{(int64_t)ival};
}

#define imm_ret(o)                      \
  switch ( o->type() ) {                \
    case OpType::IMM_8:  return IMM_8;  \
    case OpType::IMM_16: return IMM_16; \
    case OpType::IMM_32: return IMM_32; \
    default            : return IMM_64; \
	}

#define offset_ret(o)                      \
  switch ( o->type() ) {                   \
    case OpType::IMM_8:  return OFFSET_8;  \
    case OpType::IMM_16:                   \
    case OpType::IMM_32: return OFFSET_32; \
    default            : return OFFSET_64; \
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

"$0"     { yylval.operand = &zero;  return ZERO; }
"$0x0"   { yylval.operand = &zero;  return ZERO; }
"$1"     { yylval.operand = &one;   return ONE; }
"$0x1"   { yylval.operand = &one;   return ONE; }
"$3"     { yylval.operand = &three; return THREE; }
"$0x3"   { yylval.operand = &three; return THREE; }

"$-0x"[0-9a-f]+ { yylval.operand = to_imm(yytext+2, true, true);   imm_ret(yylval.operand) }
"$-"[0-9]+      { yylval.operand = to_imm(yytext+2, false, true);  imm_ret(yylval.operand) }
"$0x"[0-9a-f]+  { yylval.operand = to_imm(yytext+1, true, false);  imm_ret(yylval.operand) }
"$"[0-9]+       { yylval.operand = to_imm(yytext+1, false, false); imm_ret(yylval.operand) }

"-0x"[0-9a-f]+ { yylval.operand = to_imm(yytext+1, true, true);  offset_ret(yylval.operand) }
"-"[0-9]+      { yylval.operand = to_imm(yytext+1, false, true); offset_ret(yylval.operand) }
"0x"[0-9a-f]+  { yylval.operand = to_imm(yytext, true, false);   offset_ret(yylval.operand) }
[0-9]+         { yylval.operand = to_imm(yytext, false, false);  offset_ret(yylval.operand) }

"<66>"   { yylval.operand = &pref_66; return PREF_66; }
"<rexw>" { yylval.operand = &pref_rex_w; return PREF_REX_W; }
"<far>"  { yylval.operand = &far; return FAR; }

"%mm0"   { yylval.operand = &mm0; return MM; }
"%mm1"   { yylval.operand = &mm1; return MM; }
"%mm2"   { yylval.operand = &mm2; return MM; }
"%mm3"   { yylval.operand = &mm3; return MM; }
"%mm4"   { yylval.operand = &mm4; return MM; }
"%mm5"   { yylval.operand = &mm5; return MM; }
"%mm6"   { yylval.operand = &mm6; return MM; }
"%mm7"   { yylval.operand = &mm7; return MM; }

"%al"    { yylval.operand = &al; return AL; }
"%cl"    { yylval.operand = &cl; return CL; }
"%dl"    { yylval.operand = &dl; return RL; }
"%bl"    { yylval.operand = &bl; return RL; }

"%ah"    { yylval.operand = &ah; return RH; }
"%ch"    { yylval.operand = &ch; return RH; }
"%dh"    { yylval.operand = &dh; return RH; }
"%bh"    { yylval.operand = &bh; return RH; }
 
"%spl"   { yylval.operand = &spl;  return RB; }
"%bpl"   { yylval.operand = &bpl;  return RB; }
"%sil"   { yylval.operand = &sil;  return RB; }
"%dil"   { yylval.operand = &dil;  return RB; }
"%r8b"   { yylval.operand = &r8b;  return RB; }
"%r9b"   { yylval.operand = &r9b;  return RB; }
"%r10b"  { yylval.operand = &r10b; return RB; }
"%r11b"  { yylval.operand = &r11b; return RB; }
"%r12b"  { yylval.operand = &r12b; return RB; }
"%r13b"  { yylval.operand = &r13b; return RB; }
"%r14b"  { yylval.operand = &r14b; return RB; }
"%r15b"  { yylval.operand = &r15b; return RB; }

"%ax"    { yylval.operand = &ax;   return AX; }
"%cx"    { yylval.operand = &cx;   return CX; }
"%dx"    { yylval.operand = &dx;   return R_16; }
"%bx"    { yylval.operand = &bx;   return R_16; }
"%sp"    { yylval.operand = &sp;   return R_16; }
"%bp"    { yylval.operand = &bp;   return R_16; }
"%si"    { yylval.operand = &si;   return R_16; }
"%di"    { yylval.operand = &di;   return R_16; }
"%r8w"   { yylval.operand = &r8w;  return R_16; }
"%r9w"   { yylval.operand = &r9w;  return R_16; }
"%r10w"  { yylval.operand = &r10w; return R_16; }
"%r11w"  { yylval.operand = &r11w; return R_16; }
"%r12w"  { yylval.operand = &r12w; return R_16; }
"%r13w"  { yylval.operand = &r13w; return R_16; }
"%r14w"  { yylval.operand = &r14w; return R_16; }
"%r15w"  { yylval.operand = &r15w; return R_16; }

"%eax"   { yylval.operand = &eax; return AX; }
"%ecx"   { yylval.operand = &ecx; return R_32; }
"%edx"   { yylval.operand = &edx; return R_32; }
"%ebx"   { yylval.operand = &ebx; return R_32; }
"%esp"   { yylval.operand = &esp; return R_32; }
"%ebp"   { yylval.operand = &ebp; return R_32; }
"%esi"   { yylval.operand = &esi; return R_32; }
"%edi"   { yylval.operand = &edi; return R_32; }
"%r8d"   { yylval.operand = &r8d;  return R_32; }
"%r9d"   { yylval.operand = &r9d;  return R_32; }
"%r10d"  { yylval.operand = &r10d; return R_32; }
"%r11d"  { yylval.operand = &r11d; return R_32; }
"%r12d"  { yylval.operand = &r12d; return R_32; }
"%r13d"  { yylval.operand = &r13d; return R_32; }
"%r14d"  { yylval.operand = &r14d; return R_32; }
"%r15d"  { yylval.operand = &r15d; return R_32; }

"%rax"   { yylval.operand = &rax; return RAX; }
"%rcx"   { yylval.operand = &rcx; return R_64; }
"%rdx"   { yylval.operand = &rdx; return R_64; }
"%rbx"   { yylval.operand = &rbx; return R_64; }
"%rsp"   { yylval.operand = &rsp; return R_64; }
"%rbp"   { yylval.operand = &rbp; return R_64; }
"%rsi"   { yylval.operand = &rsi; return R_64; }
"%rdi"   { yylval.operand = &rdi; return R_64; }
"%r8"    { yylval.operand = &r8;  return R_64; }
"%r9"    { yylval.operand = &r9;  return R_64; }
"%r10"   { yylval.operand = &r10; return R_64; }
"%r11"   { yylval.operand = &r11; return R_64; }
"%r12"   { yylval.operand = &r12; return R_64; }
"%r13"   { yylval.operand = &r13; return R_64; }
"%r14"   { yylval.operand = &r14; return R_64; }
"%r15"   { yylval.operand = &r15; return R_64; }

"%es"    { yylval.operand = &es; return SREG; }
"%cs"    { yylval.operand = &cs; return SREG; }
"%ss"    { yylval.operand = &ss; return SREG; }
"%ds"    { yylval.operand = &ds; return SREG; }
"%fs"    { yylval.operand = &fs; return FS; }
"%gs"    { yylval.operand = &gs; return GS; }

"%st"    { yylval.operand = &st0; return ST_0; }
"%st(0)" { yylval.operand = &st0; return ST; }
"%st(1)" { yylval.operand = &st1; return ST; }
"%st(2)" { yylval.operand = &st2; return ST; }
"%st(3)" { yylval.operand = &st3; return ST; }
"%st(4)" { yylval.operand = &st4; return ST; }
"%st(5)" { yylval.operand = &st5; return ST; }
"%st(6)" { yylval.operand = &st6; return ST; }
"%st(7)" { yylval.operand = &st7; return ST; }

"%xmm0"  { yylval.operand = &xmm0;  return XMM_0; }
"%xmm1"  { yylval.operand = &xmm1;  return XMM; }
"%xmm2"  { yylval.operand = &xmm2;  return XMM; }
"%xmm3"  { yylval.operand = &xmm3;  return XMM; }
"%xmm4"  { yylval.operand = &xmm4;  return XMM; }
"%xmm5"  { yylval.operand = &xmm5;  return XMM; }
"%xmm6"  { yylval.operand = &xmm6;  return XMM; }
"%xmm7"  { yylval.operand = &xmm7;  return XMM; }
"%xmm8"  { yylval.operand = &xmm8;  return XMM; }
"%xmm9"  { yylval.operand = &xmm9;  return XMM; }
"%xmm10" { yylval.operand = &xmm10; return XMM; }
"%xmm11" { yylval.operand = &xmm11; return XMM; }
"%xmm12" { yylval.operand = &xmm12; return XMM; }
"%xmm13" { yylval.operand = &xmm13; return XMM; }
"%xmm14" { yylval.operand = &xmm14; return XMM; }
"%xmm15" { yylval.operand = &xmm15; return XMM; }

"%ymm0"  { yylval.operand = &ymm0;  return YMM; }
"%ymm1"  { yylval.operand = &ymm1;  return YMM; }
"%ymm2"  { yylval.operand = &ymm2;  return YMM; }
"%ymm3"  { yylval.operand = &ymm3;  return YMM; }
"%ymm4"  { yylval.operand = &ymm4;  return YMM; }
"%ymm5"  { yylval.operand = &ymm5;  return YMM; }
"%ymm6"  { yylval.operand = &ymm6;  return YMM; }
"%ymm7"  { yylval.operand = &ymm7;  return YMM; }
"%ymm8"  { yylval.operand = &ymm8;  return YMM; }
"%ymm9"  { yylval.operand = &ymm9;  return YMM; }
"%ymm10" { yylval.operand = &ymm10; return YMM; }
"%ymm11" { yylval.operand = &ymm11; return YMM; }
"%ymm12" { yylval.operand = &ymm12; return YMM; }
"%ymm13" { yylval.operand = &ymm13; return YMM; }
"%ymm14" { yylval.operand = &ymm14; return YMM; }
"%ymm15" { yylval.operand = &ymm15; return YMM; }

"adcb"   { return ADCB; }
"adcw"   { return ADCW; }

. { yyterminate(); }

%%
