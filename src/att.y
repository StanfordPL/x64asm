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

This parser has to deal with the fact that x86 assembly isn't LALR(1)
parseable.  The high level way around this is to parse opcode, operand
string pairs and then use those pairs as keys in a lookup table to resolve
instruction form. 

A side effect of this implementation is that some of the parsing rules for 
memory operands shadow the parsing rules for moffs and rel operands.  This, 
combined with the fact that the corresponding lexer stores least specific
operand types is handled using a method to check whether operands can be 
reinterpreted as the desired types and cast as necessary.

This implementation relies on a table which is ordered such that most 
specific operand orderings appear prior to less specific orderings, so that
for instance, adc $0x10, %al is parsed as ADC_IMM8_AL rather than ADC_IMM32_R32.
 */

#include <map>
#include <sstream>
#include <string>
#include <vector>

#include "src/code.h"
#include "src/env_reg.h"
#include "src/instruction.h"
#include "src/label.h"
#include "src/m.h"
#include "src/moffs.h"
#include "src/opcode.h"
#include "src/rel.h"
#include "src/type.h"

using namespace std;
using namespace x64asm;

extern int yylex();

typedef std::pair<x64asm::Opcode, std::vector<x64asm::Type>> Entry;
typedef std::vector<Entry> Row;
typedef std::map<std::string, Row> Table;

void yyerror(std::istream& is, x64asm::Code& code, const char* s);

Table att_table = {
	#include "src/att.table"
};

bool is_mem(Type t) {
	return t == Type::M_8           || t == Type::M_16          || 
		     t == Type::M_32          || t == Type::M_64          || 
				 t == Type::M_128         || t == Type::M_256         ||
				 t == Type::M_16_INT      || t == Type::M_32_INT      || 
				 t == Type::M_64_INT      || t == Type::M_32_FP       || 
				 t == Type::M_64_FP       || t == Type::M_80_FP       ||
				 t == Type::M_80_BCD      || t == Type::M_2_BYTE      || 
				 t == Type::M_28_BYTE     || t == Type::M_108_BYTE    || 
				 t == Type::M_512_BYTE    || t == Type::FAR_PTR_16_16 ||
		     t == Type::FAR_PTR_16_32 || t == Type::FAR_PTR_16_64;
}

bool is_a(const Operand* o, Type parse, Type target) {
	// These first two parses still have placeholder types.
	// They should be checked before the generic equality tests.
	if ( parse == Type::IMM_8 )
		switch ( target ) {
			case Type::ZERO:   return ((const Zero*)o)->check();
			case Type::ONE:    return ((const One*)o)->check();
			case Type::THREE:  return ((const Three*)o)->check();
			case Type::IMM_8:  return ((const Imm8*)o)->check();
			case Type::IMM_16: return ((const Imm16*)o)->check();
			case Type::IMM_32: return ((const Imm32*)o)->check();
			case Type::IMM_64: return ((const Imm64*)o)->check();
			default:           return false;
		}

	if ( parse == Type::MOFFS_8 ) {
		const auto offs = ((Moffs8*)o)->get_offset();
		if ( target == Type::MOFFS_8 || target == Type::MOFFS_16 ||
				 target == Type::MOFFS_32 || target == Type::MOFFS_64 )
			return true;
		if ( is_mem(target) || target == Type::REL_32 )
			return ((const Imm32*)&offs)->check();
		if ( target == Type::REL_8 )
			return ((const Imm8*)&offs)->check();
	}

	// Now it's alright to perform the generic checks.
	if ( parse == target )
		return true;
	if ( is_mem(parse) && is_mem(target) )
		return true;

	// Now try simple promotions.
	if ( parse == Type::R_8 ) {
		if ( target == Type::AL )
			return ((const Al*)o)->check();
		if ( target == Type::CL )
			return ((const Cl*)o)->check();
	}
	
	if ( parse == Type::R_16 ) {
		if ( target == Type::AX )
			return ((const Ax*)o)->check();
		if ( target == Type::DX )
			return ((const Dx*)o)->check();
	}

	if ( parse == Type::R_32 && target == Type::EAX )
			return ((const Eax*)o)->check();

	if ( parse == Type::R_64 && target == Type::RAX )
			return ((const Rax*)o)->check();

	if ( parse == Type::SREG ) {
		if ( target == Type::FS )
			return ((const Fs*)o)->check();
		if ( target == Type::GS )
			return ((const Gs*)o)->check();
	}

	if ( parse == Type::ST && target == Type::ST_0 )
			return ((const St0*)o)->check();

	if ( parse == Type::XMM && target == Type::XMM_0 )
			return ((const Xmm0*)o)->check();

	return false;
}

const Operand promote(const Operand* op, Type parse, Type target) {
	if ( parse == Type::MOFFS_8 ) {
		const auto moffs = (Moffs8*)op;
		const auto offs = moffs->get_offset();

		if ( is_mem(target) ) {
			M8 ret{*((Imm32*)(&offs))};
			if ( moffs->contains_seg() )
	  		ret.set_seg(moffs->get_seg());
			return ret;
		}
		if ( target == Type::REL_8 || target == Type::REL_32 )
			return offs;
	} else if (is_mem(parse)) {
    M8 ret(*(reinterpret_cast<const M8*>(op)), target);
    return ret;
  }

	return *op;
}

// Returns a poorly formed instruction on error
const Instruction* to_instr(const std::string& opc, 
    const std::vector<std::pair<const Operand*, x64asm::Type>*>& ops) {

	//cout << "Opcode: " << opc << endl;
	const auto itr = att_table.find(opc);
	if ( itr == att_table.end() )
		return new Instruction{x64asm::ADC_R16_R16, {Imm8{64},Imm8{64}}};

	for ( const auto& entry : itr->second ) {
		const auto arity = entry.second.size();
		if ( ops.size() != arity )
			continue;

		auto match = true;
		for ( size_t i = 0; i < arity; ++i ) {
			//cout << "Attempting to match " << (int)ops[i]->second << " to " << (int)entry.second[i] << endl;
			match &= is_a(ops[i]->first, ops[i]->second, entry.second[i]);
			if ( !match )
				break;
		}
		//cout << (match ? "OKAY" : "FAILED") << endl;

		if ( !match )
			continue;

		Instruction* instr = new Instruction{entry.first};
		for ( size_t i = 0, ie = ops.size(); i < ie; ++i ) {
			auto op = promote(ops[i]->first, ops[i]->second, entry.second[i]);
			instr->set_operand(i, op);
		}
		return instr;
	}

	return new Instruction{x64asm::ADC_R16_R16, {Imm8{64},Imm8{64}}};
}

R32 base32(const Operand* o) { 
	const auto ret = *(static_cast<const R32*>(o));
	delete o;
	return ret;
}

R64 base64(const Operand* o) { 
	const auto ret = *(static_cast<const R64*>(o));
	delete o;
	return ret;
}

R32 index32(const Operand* o) { 
	const auto ret = *(static_cast<const R32*>(o));
	delete o;
	return ret;
}

R64 index64(const Operand* o) { 
	const auto ret = *(static_cast<const R64*>(o));
	delete o;
	return ret;
}

Imm32 disp(const Operand* o) { 
	const auto ret = *(static_cast<const Imm32*>(o));
	delete o;
	return ret;
}

Sreg seg(const Operand* o) { 
	const auto ret = *(static_cast<const Sreg*>(o));
	delete o;
	return ret;
}

Imm64 offset(const Operand* o) { 
	const auto ret = *(static_cast<const Imm64*>(o));
	delete o;
	return ret;
}

%}

%code requires {
  #include "src/instruction.h"
	#include "src/code.h"
	#include "src/env_reg.h"
	#include "src/operand.h"
	#include "src/m.h"
	#include "src/moffs.h"
	#include "src/rel.h"
}

%union {
	x64asm::Scale scale;
	const x64asm::Rip* rip;
	const x64asm::Operand* operand;
	std::pair<const x64asm::Operand*, x64asm::Type>* typed_operand;
	std::vector<std::pair<const Operand*, x64asm::Type>*>* typed_operands;
	const std::string* opcode;
	const x64asm::Instruction* instr;
  std::vector<x64asm::Instruction>* instrs;
}

%token <int> COMMA
%token <int> COLON
%token <int> OPEN
%token <int> CLOSE
%token <int> ENDL

%token <scale>  SCALE
%token <rip>    RIP

%token <operand> HINT
%token <operand> IMM
%token <operand> OFFSET
%token <operand> LABEL
%token <operand> PREF_66
%token <operand> PREF_REX_W
%token <operand> FAR
%token <operand> MM
%token <operand> RH
%token <operand> R_8
%token <operand> R_16
%token <operand> R_32
%token <operand> R_64
%token <operand> SREG
%token <operand> ST
%token <operand> XMM
%token <operand> YMM

%token <opcode> OPCODE

%type <operand> moffs
%type <operand> m

%type <typed_operand>  typed_operand
%type <typed_operands> typed_operands
%type <instr>          instr
%type <instrs>         instrs

%locations
%error-verbose

%parse-param { std::istream& is }
%parse-param { x64asm::Code& code }

%start code

%%

blank : /* empty */ | blank ENDL { }

code : blank instrs { 
	code.assign($2->begin(), $2->end()); delete $2; 
}

instrs : /* empty */ {
	$$ = new vector<Instruction>();
}
| instr { 
  $$ = new vector<Instruction>(); 
	$$->push_back(*$1); 
	delete $1; 
} 
| instrs instr { 
	$1->push_back(*$2); 
	delete($2); 
}

instr : LABEL COLON ENDL blank {
  $$ = new Instruction{Opcode::LABEL_DEFN, {*$1}};
	delete $1;
}
| OPCODE typed_operands ENDL blank {
	$$ = to_instr(*$1, *$2);
	if ( !$$->check() ) {
		ostringstream oss;
		oss << "Unable to parse instruction (";
		oss << *$1;
		oss << ")";
		yyerror(is, code, oss.str().c_str());
	}

	delete $1;
	for ( const auto op : *$2 ) {
		delete op->first;
		delete op;
	}
	delete $2;
}

typed_operands : /* empty */ { 
	$$ = new vector<pair<const Operand*, Type>*>(); 
}
| typed_operand { 
	$$ = new vector<pair<const Operand*, Type>*>(); 
	$$->push_back($1); 
}
| typed_operand COMMA typed_operand {
	$$ = new vector<pair<const Operand*, Type>*>(); 
	$$->push_back($3);
	$$->push_back($1);
}
| typed_operand COMMA typed_operand COMMA typed_operand { 
	$$ = new vector<pair<const Operand*, Type>*>(); 
	$$->push_back($5); 
	$$->push_back($3); 
	$$->push_back($1); 
} 
| typed_operand COMMA typed_operand COMMA typed_operand COMMA typed_operand { 
	$$ = new vector<pair<const Operand*, Type>*>(); 
	$$->push_back($7); 
	$$->push_back($5); 
	$$->push_back($3); 
	$$->push_back($1); 
} 

typed_operand : HINT { $$ = new pair<const Operand*, Type>{$1, Type::HINT}; }
	| IMM { $$ = new pair<const Operand*, Type>{$1, Type::IMM_8}; }
	| LABEL { $$ = new pair<const Operand*, Type>{$1, Type::LABEL}; }
	| PREF_66 { $$ = new pair<const Operand*, Type>{$1, Type::PREF_66}; }
	| PREF_REX_W { $$ = new pair<const Operand*, Type>{$1, Type::PREF_REX_W}; }
	| FAR { $$ = new pair<const Operand*, Type>{$1, Type::FAR}; }
	| MM { $$ = new pair<const Operand*, Type>{$1, Type::MM}; }
	| RH { $$ = new pair<const Operand*, Type>{$1, Type::RH}; }
	| R_8 { $$ = new pair<const Operand*, Type>{$1, Type::R_8}; }
	| R_16 { $$ = new pair<const Operand*, Type>{$1, Type::R_16}; }
	| R_32 { $$ = new pair<const Operand*, Type>{$1, Type::R_32}; }
	| R_64 { $$ = new pair<const Operand*, Type>{$1, Type::R_64}; }
	| SREG { $$ = new pair<const Operand*, Type>{$1, Type::SREG}; }
	| ST { $$ = new pair<const Operand*, Type>{$1, Type::ST}; }
	| XMM { $$ = new pair<const Operand*, Type>{$1, Type::XMM}; }
	| YMM { $$ = new pair<const Operand*, Type>{$1, Type::YMM}; }
	| moffs { $$ = new pair<const Operand*, Type>{$1, Type::MOFFS_8}; }
	| m { $$ = new pair<const Operand*, Type>{$1, Type::M_8}; }

moffs :
  OFFSET { $$ = new Moffs8(offset($1)); }
| SREG COLON OFFSET { $$ = new Moffs8(seg($1), offset($3)); }

m : 
  OPEN R_32 CLOSE { $$ = new M8(base32($2)); }
| OPEN R_64 CLOSE { $$ = new M8(base64($2)); }
| OPEN RIP CLOSE { $$ = new M8(Constants::rip()); }
| SREG COLON OPEN R_32 CLOSE { $$ = new M8(seg($1), base32($4)); }
| SREG COLON OPEN R_64 CLOSE { $$ = new M8(seg($1), base64($4)); }
| SREG COLON OPEN RIP CLOSE { $$ = new M8(seg($1), Constants::rip()); }
| OFFSET OPEN R_32 CLOSE { $$ = new M8(base32($3), disp($1)); }
| OFFSET OPEN R_64 CLOSE { $$ = new M8(base64($3), disp($1)); }
| OFFSET OPEN RIP CLOSE { $$ = new M8(Constants::rip(), disp($1)); }
| SREG COLON OFFSET OPEN R_32 CLOSE { $$ = new M8(seg($1), base32($5), disp($3)); }
| SREG COLON OFFSET OPEN R_64 CLOSE { $$ = new M8(seg($1), base64($5), disp($3)); }
| SREG COLON OFFSET OPEN RIP CLOSE { $$ = new M8(seg($1), Constants::rip(), disp($3)); }
| OPEN COMMA R_32 COMMA SCALE CLOSE { $$ = new M8(index32($3), $5); }
| OPEN COMMA R_64 COMMA SCALE CLOSE { $$ = new M8(index64($3), $5); }
| OPEN COMMA R_32 CLOSE { $$ = new M8(index32($3), Scale::TIMES_1); }
| OPEN COMMA R_64 CLOSE { $$ = new M8(index64($3), Scale::TIMES_1); }
| SREG COLON OPEN COMMA R_32 COMMA SCALE CLOSE { $$ = new M8(seg($1), index32($5), $7); }
| SREG COLON OPEN COMMA R_64 COMMA SCALE CLOSE { $$ = new M8(seg($1), index64($5), $7); }
| SREG COLON OPEN COMMA R_32 CLOSE { $$ = new M8(seg($1), index32($5), Scale::TIMES_1); }
| SREG COLON OPEN COMMA R_64 CLOSE { $$ = new M8(seg($1), index64($5), Scale::TIMES_1); }
| OFFSET OPEN COMMA R_32 COMMA SCALE CLOSE { $$ = new M8(index32($4), $6, disp($1)); }
| OFFSET OPEN COMMA R_64 COMMA SCALE CLOSE { $$ = new M8(index64($4), $6, disp($1)); }
| OFFSET OPEN COMMA R_32 CLOSE { $$ = new M8(index32($4), Scale::TIMES_1, disp($1)); }
| OFFSET OPEN COMMA R_64 CLOSE { $$ = new M8(index64($4), Scale::TIMES_1, disp($1)); }
| SREG COLON OFFSET OPEN COMMA R_32 COMMA SCALE CLOSE { $$ = new M8(seg($1), index32($6), $8, disp($3)); }
| SREG COLON OFFSET OPEN COMMA R_64 COMMA SCALE CLOSE { $$ = new M8(seg($1), index64($6), $8, disp($3)); }
| SREG COLON OFFSET OPEN COMMA R_32 CLOSE { $$ = new M8(seg($1), index32($6), Scale::TIMES_1, disp($3)); }
| SREG COLON OFFSET OPEN COMMA R_64 CLOSE { $$ = new M8(seg($1), index64($6), Scale::TIMES_1, disp($3)); }
| OPEN R_32 COMMA R_32 COMMA SCALE CLOSE { $$ = new M8(base32($2), index32($4), $6); }
| OPEN R_64 COMMA R_64 COMMA SCALE CLOSE { $$ = new M8(base64($2), index64($4), $6); }
| OPEN R_32 COMMA R_32 CLOSE { $$ = new M8(base32($2), index32($4), Scale::TIMES_1); }
| OPEN R_64 COMMA R_64 CLOSE { $$ = new M8(base64($2), index64($4), Scale::TIMES_1); }
| SREG COLON OPEN R_32 COMMA R_32 COMMA SCALE CLOSE { $$ = new M8(seg($1), base32($4), index32($6), $8); }
| SREG COLON OPEN R_64 COMMA R_64 COMMA SCALE CLOSE { $$ = new M8(seg($1), base64($4), index64($6), $8); }
| SREG COLON OPEN R_32 COMMA R_32 CLOSE { $$ = new M8(seg($1), base32($4), index32($6), Scale::TIMES_1); }
| SREG COLON OPEN R_64 COMMA R_64 CLOSE { $$ = new M8(seg($1), base64($4), index64($6), Scale::TIMES_1); }
| OFFSET OPEN R_32 COMMA R_32 COMMA SCALE CLOSE { $$ = new M8(base32($3), index32($5), $7, disp($1)); }
| OFFSET OPEN R_64 COMMA R_64 COMMA SCALE CLOSE { $$ = new M8(base64($3), index64($5), $7, disp($1)); }
| OFFSET OPEN R_32 COMMA R_32 CLOSE { $$ = new M8(base32($3), index32($5), Scale::TIMES_1, disp($1)); }
| OFFSET OPEN R_64 COMMA R_64 CLOSE { $$ = new M8(base64($3), index64($5), Scale::TIMES_1, disp($1)); }
| SREG COLON OFFSET OPEN R_32 COMMA R_32 COMMA SCALE CLOSE { $$ = new M8(seg($1), base32($5), index32($7), $9, disp($3)); }
| SREG COLON OFFSET OPEN R_64 COMMA R_64 COMMA SCALE CLOSE { $$ = new M8(seg($1), base64($5), index64($7), $9, disp($3)); }
| SREG COLON OFFSET OPEN R_32 COMMA R_32 CLOSE { $$ = new M8(seg($1), base32($5), index32($7), Scale::TIMES_1, disp($3)); }
| SREG COLON OFFSET OPEN R_64 COMMA R_64 CLOSE { $$ = new M8(seg($1), base64($5), index64($7), Scale::TIMES_1, disp($3)); }

%% 

void yyerror(std::istream& is, x64asm::Code& code, const char* s) { 
	is.setstate(std::ios::failbit); 
	cerr << "Error on line " << yylloc.first_line << ": "  << s << endl;
}
