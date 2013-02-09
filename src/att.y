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
#include <string>
#include <vector>

#include "src/code.h"
#include "src/env_reg.h"
#include "src/instruction.h"
#include "src/label.h"
#include "src/m.h"
#include "src/moffs.h"
#include "src/opcode.h"
#include "src/op_type.h"
#include "src/rel.h"

using namespace std;
using namespace x64asm;

extern int yylex();
extern int yy_line_number;

void yyerror(std::istream& is, x64asm::Code& code, const char* s) { 
	is.setstate(std::ios::failbit); 
	cerr << "Error on line " << yy_line_number << ": "  << s << endl;
}

R32 base32(const Operand* o) { 
	return *((R32*)o);
}

R64 base64(const Operand* o) { 
	return *((R64*)o);
}

R32 index32(const Operand* o) { 
	return *((R32*)o);
}

R64 index64(const Operand* o) { 
	return *((R64*)o);
}

Imm32 disp(uint64_t o) { 
	return Imm32{o};
}

Sreg seg(const Operand* o) { 
	return *((Sreg*)o);
}

Imm64 offset(uint64_t o) { 
	return Imm64{o};
}

typedef std::pair<x64asm::Opcode, std::vector<x64asm::OpType>> Entry;
typedef std::vector<Entry> Row;
typedef std::map<const char*, Row> Table;

Table att_table = {
	#include "src/att.table"
};

bool is_a(OpType a, OpType b) {
	switch ( b ) {
		case OpType::IMM_8: 
			return a == OpType::ZERO   ||
						 a == OpType::ONE    ||
						 a == OpType::THREE  ||
						 a == OpType::IMM_8;
		case OpType::IMM_16: 
			return a == OpType::ZERO   ||
						 a == OpType::ONE    ||
						 a == OpType::THREE  ||
						 a == OpType::IMM_8  ||
						 a == OpType::IMM_16;

		//      -- Stupid LALR parser
		// TODO -- Can promote offset-only memory form to rel_8 / rel_32	
		// TODO -- Can promote offset-only or seg:offset to moffs8/16/32/64
		default:
			return a == b;	 
	}
}

template <typename T>
T promote_to(const M* m) {
	T t{Imm32{0}};
	if ( m->contains_seg() )
		t.set_seg(m->get_seg());
	if ( m->contains_base() )
		t.set_base(m->get_base());
	if ( m->contains_index() )
		t.set_index(m->get_index());

	t.set_scale(m->get_scale());
	t.set_disp(m->get_disp());
	t.set_addr_or(m->get_addr_or());
	t.set_rip_offset(m->rip_offset());

	return t;
}

template <typename T>
T promote_to(const Moffs* m) {
	if ( m->contains_seg() )
		return T{m->get_seg(), m->get_offset()};
	else
		return T{m->get_offset()};
}

// Returns a poorly formed instruction on error
const Instruction* to_instr(const std::string& opc, 
                            const std::vector<const Operand*>& ops) {
	const auto itr = att_table.find(opc.c_str());
	if ( itr == att_table.end() )
		return new Instruction{x64asm::NOP, {xmm0}};

	for ( const auto& entry : itr->second ) {
		const auto arity = entry.second.size();
		if ( ops.size() != arity )
			continue;

		auto match = true;
		for ( size_t i = 0; i < arity; ++i ) 
			match &= is_a(ops[i]->type(), entry.second[i]);

		if ( match ) {
			// Promotion goes here.
			Instruction* instr = new Instruction{entry.first};
			for ( size_t i = 0, ie = ops.size(); i < ie; ++i )
				instr->set_operand(i, *ops[i]);

			return instr;
		}
	}

	return new Instruction{x64asm::NOP, {xmm0}};
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
	uint64_t offset;
	const x64asm::Rip* rip;
	const x64asm::Operand* operand;
	std::vector<const Operand*>* operands;
	const std::string* opcode;
	const x64asm::Instruction* instr;
  std::vector<x64asm::Instruction>* instrs;
}

%token <int> COMMA
%token <int> COLON
%token <int> OPEN
%token <int> CLOSE
%token <int> ENDL

%token <offset> OFFSET
%token <scale>  SCALE
%token <rip>    RIP

%token <operand> HINT
%token <operand> ZERO 
%token <operand> ONE 
%token <operand> THREE 
%token <operand> IMM_8 
%token <operand> IMM_16
%token <operand> IMM_32 
%token <operand> IMM_64
%token <operand> LABEL
%token <operand> PREF_66
%token <operand> PREF_REX_W
%token <operand> FAR
%token <operand> MM
%token <operand> AL
%token <operand> CL
%token <operand> RL
%token <operand> RH
%token <operand> RB
%token <operand> AX
%token <operand> DX
%token <operand> R_16
%token <operand> EAX
%token <operand> R_32
%token <operand> RAX
%token <operand> R_64
%token <operand> SREG
%token <operand> FS
%token <operand> GS
%token <operand> ST_0
%token <operand> ST
%token <operand> XMM_0
%token <operand> XMM
%token <operand> YMM

%token <opcode> OPCODE

%type <operand>  m

%type <operand>  operand
%type <operands> operands
%type <instr>    instr
%type <instrs>   instrs

%locations
%error-verbose

%parse-param { std::istream& is }
%parse-param { x64asm::Code& code }

%start code

%%

m : 
  OFFSET { $$ = new M8{disp($1)}; }
| SREG COLON OFFSET { $$ = new M8{seg($1), disp($3)}; }
| OPEN R_32 CLOSE { $$ = new M8{base32($2)}; }
| OPEN R_64 CLOSE { $$ = new M8{base64($2)}; }
| OPEN RIP CLOSE { $$ = new M8{rip}; }
| SREG COLON OPEN R_32 CLOSE { $$ = new M8{seg($1), base32($4)}; }
| SREG COLON OPEN R_64 CLOSE { $$ = new M8{seg($1), base64($4)}; }
| SREG COLON OPEN RIP CLOSE { $$ = new M8{seg($1), rip}; }
| OFFSET OPEN R_32 CLOSE { $$ = new M8{base32($3), disp($1)}; }
| OFFSET OPEN R_64 CLOSE { $$ = new M8{base64($3), disp($1)}; }
| OFFSET OPEN RIP CLOSE { $$ = new M8{rip, disp($1)}; }
| SREG COLON OFFSET OPEN R_32 CLOSE { $$ = new M8{seg($1), base32($5), disp($3)}; }
| SREG COLON OFFSET OPEN R_64 CLOSE { $$ = new M8{seg($1), base64($5), disp($3)}; }
| SREG COLON OFFSET OPEN RIP CLOSE { $$ = new M8{seg($1), rip, disp($3)}; }
| OPEN R_32 COMMA SCALE CLOSE { $$ = new M8{index32($2), $4}; }
| OPEN R_64 COMMA SCALE CLOSE { $$ = new M8{index64($2), $4}; }
| SREG COLON OPEN R_32 COMMA SCALE CLOSE { $$ = new M8{seg($1), index32($4), $6}; }
| SREG COLON OPEN R_64 COMMA SCALE CLOSE { $$ = new M8{seg($1), index64($4), $6}; }
| OFFSET OPEN R_32 COMMA SCALE CLOSE { $$ = new M8{index32($3), $5, disp($1)}; }
| OFFSET OPEN R_64 COMMA SCALE CLOSE { $$ = new M8{index64($3), $5, disp($1)}; }
| SREG COLON OFFSET OPEN R_32 COMMA SCALE CLOSE { $$ = new M8{seg($1), index32($5), $7, disp($3)}; }
| SREG COLON OFFSET OPEN R_64 COMMA SCALE CLOSE { $$ = new M8{seg($1), index64($5), $7, disp($3)}; }
| OPEN R_32 COMMA R_32 COMMA SCALE CLOSE { $$ = new M8{base32($2), index32($4), $6}; }
| OPEN R_64 COMMA R_64 COMMA SCALE CLOSE { $$ = new M8{base64($2), index64($4), $6}; }
| SREG COLON OPEN R_32 COMMA R_32 COMMA SCALE CLOSE { $$ = new M8{seg($1), base32($4), index32($6), $8}; }
| SREG COLON OPEN R_64 COMMA R_64 COMMA SCALE CLOSE { $$ = new M8{seg($1), base64($4), index64($6), $8}; }
| OFFSET OPEN R_32 COMMA R_32 COMMA SCALE CLOSE { $$ = new M8{base32($3), index32($5), $7, disp($1)}; }
| OFFSET OPEN R_64 COMMA R_64 COMMA SCALE CLOSE { $$ = new M8{base64($3), index64($5), $7, disp($1)}; }
| SREG COLON OFFSET OPEN R_32 COMMA R_32 COMMA SCALE CLOSE { $$ = new M8{seg($1), base32($5), index32($7), $9, disp($3)}; }
| SREG COLON OFFSET OPEN R_64 COMMA R_64 COMMA SCALE CLOSE { $$ = new M8{seg($1), base64($5), index64($7), $9, disp($3)}; }

blank : /* empty */ | blank ENDL { }

code : blank instrs { 
	code.assign($2->begin(), $2->end()); delete $2; 
}

instrs : instr { 
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
}
| OPCODE operands ENDL blank {
	$$ = to_instr(*$1, *$2);

	for ( const auto o : *$2 ) {
		if ( dynamic_cast<const Imm*>(o) != 0 )
			delete o;
		if ( dynamic_cast<const Label*>(o) != 0 )
			delete o;
		if ( dynamic_cast<const M*>(o) != 0 )
			delete o;
	}
		
	delete $1;
	delete $2;

	if ( !$$->check() )
		yyerror(is, code, "Unable to parse instruction!");
}

operands : /* empty */ { 
	$$ = new vector<const Operand*>(); 
}
| operand { 
	$$ = new vector<const Operand*>(); 
	$$->push_back($1); 
}
| operand COMMA operand {
	$$ = new vector<const Operand*>();
	$$->push_back($3);
	$$->push_back($1);
}
| operand COMMA operand COMMA operand { 
	$$ = new vector<const Operand*>(); 
	$$->push_back($5); 
	$$->push_back($3); 
	$$->push_back($1); 
} 
| operand COMMA operand COMMA operand COMMA operand { 
	$$ = new vector<const Operand*>(); 
	$$->push_back($7); 
	$$->push_back($5); 
	$$->push_back($3); 
	$$->push_back($1); 
} 

operand : 
	HINT |
	ZERO | ONE | THREE | IMM_8 | IMM_16 | IMM_32 | IMM_64 |
	LABEL |
  PREF_66 | PREF_REX_W | FAR |
	MM |
	AL | CL | RL | RH | RB | AX | DX | R_16 | EAX | R_32 | RAX | R_64 |
	SREG | FS | GS |
	ST_0 | ST |
	XMM_0 | XMM |
	YMM |
	m {
	$$ = $1;
}

%% 
