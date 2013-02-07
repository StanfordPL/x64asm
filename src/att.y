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

const AddrR* base(const Operand* o) { 
	return dynamic_cast<const AddrR*>(o); 
}

const AddrR* index(const Operand* o) { 
	return dynamic_cast<const AddrR*>(o); 
}

const Imm32* disp(uint64_t o) { 
	return new Imm32{o};
}

const Sreg* seg(const Operand* o) { 
	return dynamic_cast<const Sreg*>(o); 
}

const Imm64* offset(uint64_t o) { 
	return new Imm64{o};
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
const T* promote_to(const M* m) {
	T* t = new T{&rax};
	if ( m->contains_seg() )
		t->set_seg(m->get_seg());
	if ( m->contains_base() )
		t->set_base(m->get_base());
	else
		t->clear_base();
	if ( m->contains_index() ) {
		t->set_index(m->get_index());
		t->set_scale(m->get_scale());
	}
	if ( m->contains_disp() )
		t->set_disp(m->get_disp());
	t->set_addr_or(m->get_addr_or());

	return t;
}

template <typename T>
const T* promote_to(const Moffs* m) {
	if ( m->contains_seg() )
		return new T{m->get_seg(), m->get_offset()};
	else
		return new T{m->get_offset()};
}

// Returns a poorly formed instruction on error
const Instruction* to_instr(const std::string& opc, 
                            const std::vector<const Operand*>& ops) {
	const auto itr = att_table.find(opc.c_str());
	if ( itr == att_table.end() )
		return new Instruction{x64asm::NOP, {&ymm0}};

	for ( const auto& entry : itr->second ) {
		const auto arity = entry.second.size();
		if ( ops.size() != arity )
			continue;

		auto match = true;
		for ( size_t i = 0; i < arity; ++i ) 
			match &= is_a(ops[i]->type(), entry.second[i]);

		if ( match ) 
			return new Instruction{entry.first, ops.begin(), ops.end()};
	}

	return new Instruction{x64asm::NOP, {&ymm0}};
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
| OPEN R_32 CLOSE { $$ = new M8{base($2), true}; }
| OPEN R_64 CLOSE { $$ = new M8{base($2), false}; }
| OPEN RIP CLOSE { $$ = new M8{base(&rax), false}; }
| SREG COLON OPEN R_32 CLOSE { $$ = new M8{seg($1), base($4), true}; }
| SREG COLON OPEN R_64 CLOSE { $$ = new M8{seg($1), base($4), false}; }
| SREG COLON OPEN RIP CLOSE { $$ = new M8{seg($1), base(&rax), false}; }
| OFFSET OPEN R_32 CLOSE { $$ = new M8{base($3), disp($1), true}; }
| OFFSET OPEN R_64 CLOSE { $$ = new M8{base($3), disp($1), false}; }
| OFFSET OPEN RIP CLOSE { $$ = new M8{base(&rax), disp($1), false}; }
| SREG COLON OFFSET OPEN R_32 CLOSE { $$ = new M8{seg($1), base($5), disp($3), true}; }
| SREG COLON OFFSET OPEN R_64 CLOSE { $$ = new M8{seg($1), base($5), disp($3), false}; }
| SREG COLON OFFSET OPEN RIP CLOSE { $$ = new M8{seg($1), base(&rax), disp($3), false}; }
| OPEN R_32 COMMA SCALE CLOSE { $$ = new M8{index($2), $4, true}; }
| OPEN R_64 COMMA SCALE CLOSE { $$ = new M8{index($2), $4, false}; }
| SREG COLON OPEN R_32 COMMA SCALE CLOSE { $$ = new M8{seg($1), index($4), $6, true}; }
| SREG COLON OPEN R_64 COMMA SCALE CLOSE { $$ = new M8{seg($1), index($4), $6, false}; }
| OFFSET OPEN R_32 COMMA SCALE CLOSE { $$ = new M8{index($3), $5, disp($1), true}; }
| OFFSET OPEN R_64 COMMA SCALE CLOSE { $$ = new M8{index($3), $5, disp($1), false}; }
| SREG COLON OFFSET OPEN R_32 COMMA SCALE CLOSE { $$ = new M8{seg($1), index($5), $7, disp($3), true}; }
| SREG COLON OFFSET OPEN R_64 COMMA SCALE CLOSE { $$ = new M8{seg($1), index($5), $7, disp($3), false}; }
| OPEN R_32 COMMA R_32 COMMA SCALE CLOSE { $$ = new M8{base($2), index($4), $6, true}; }
| OPEN R_64 COMMA R_64 COMMA SCALE CLOSE { $$ = new M8{base($2), index($4), $6, false}; }
| SREG COLON OPEN R_32 COMMA R_32 COMMA SCALE CLOSE { $$ = new M8{seg($1), base($4), index($6), $8, true}; }
| SREG COLON OPEN R_64 COMMA R_64 COMMA SCALE CLOSE { $$ = new M8{seg($1), base($4), index($6), $8, false}; }
| OFFSET OPEN R_32 COMMA R_32 COMMA SCALE CLOSE { $$ = new M8{base($3), index($5), $7, disp($1), true}; }
| OFFSET OPEN R_64 COMMA R_64 COMMA SCALE CLOSE { $$ = new M8{base($3), index($5), $7, disp($1), false}; }
| SREG COLON OFFSET OPEN R_32 COMMA R_32 COMMA SCALE CLOSE { $$ = new M8{seg($1), base($5), index($7), $9, disp($3), true}; }
| SREG COLON OFFSET OPEN R_64 COMMA R_64 COMMA SCALE CLOSE { $$ = new M8{seg($1), base($5), index($7), $9, disp($3), false}; }

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
  $$ = new Instruction{Opcode::LABEL_DEFN, {$1}};
}
| OPCODE operands ENDL blank {
	$$ = to_instr(*$1, *$2);
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
