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

template <typename T>
const T* make_mem(const M* m) {
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
const T* make_moffs(const Moffs* m) {
	if ( m->contains_seg() )
		return new T{m->get_seg(), m->get_offset()};
	else
		return new T{m->get_offset()};
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
		default:
			return a == b;	 
	}
}

Instruction* make_instr(std::istream& is, const std::string& opc, 
                        const std::vector<const Operand*>& ops) {
	const auto itr = att_table.find(opc.c_str());
	if ( itr == att_table.end() ) {
		is.setstate(std::ios::failbit);
		cerr << "Unrecognized opcode [" << opc << "]" << endl;
		return new Instruction{x64asm::NOP, {}};
	}

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

	is.setstate(std::ios::failbit);
	cerr << "Unrecognized operand values for [" << opc << "]" << endl;
	return new Instruction{x64asm::NOP, {}};
}

const AddrR* base(const Operand* o) { return dynamic_cast<const AddrR*>(o); }
const AddrR* index(const Operand* o) { return dynamic_cast<const AddrR*>(o); }
const Imm32* disp(const Operand* o) { return dynamic_cast<const Imm32*>(o); }
const Sreg* seg(const Operand* o) { return dynamic_cast<const Sreg*>(o); }
const Imm64* offset(const Operand* o) { return dynamic_cast<const Imm64*>(o); }

%}

%code requires {
  #include "src/instruction.h"
	#include "src/code.h"
	#include "src/operand.h"
	#include "src/m.h"
	#include "src/moffs.h"
	#include "src/rel.h"
}

%union {
	x64asm::Scale scale;
	const std::string* opcode;
	const x64asm::Operand* operand;
	std::vector<const Operand*>* operands;
	x64asm::Instruction* instr;
  std::vector<x64asm::Instruction>* instrs;
}

%token <int> COMMA
%token <int> COLON
%token <int> OPEN
%token <int> CLOSE
%token <int> ENDL

%token <opcode> OPCODE

%token <operand> HINT
%token <operand> PREF_66
%token <operand> PREF_REX_W
%token <operand> FAR
%token <operand> LABEL
%token <operand> ZERO 
%token <operand> ONE 
%token <operand> THREE 
%token <operand> IMM_8 
%token <operand> IMM_16
%token <operand> IMM_32 
%token <operand> IMM_64
%token <operand> OFFSET_8
%token <operand> OFFSET_32
%token <operand> OFFSET_64
%token <scale>   SCALE
%type  <operand> M
%token <operand> MM
%type  <operand> MOFFS
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
%type  <operand> REL_8 
%type  <operand> REL_32 
%token <operand> SREG
%token <operand> FS
%token <operand> GS
%token <operand> ST_0
%token <operand> ST
%token <operand> XMM_0
%token <operand> XMM
%token <operand> YMM

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

M : 
  OFFSET_32 { $$ = new M8{disp($1)}; }
| SREG COLON OFFSET_32 { $$ = new M8{seg($1), disp($3)}; }
| OPEN R_32 CLOSE { $$ = new M8{base($2), true}; }
| OPEN R_64 CLOSE { $$ = new M8{base($2), false}; }
| SREG COLON OPEN R_32 CLOSE { $$ = new M8{seg($1), base($4), true}; }
| SREG COLON OPEN R_64 CLOSE { $$ = new M8{seg($1), base($4), false}; }
| OFFSET_32 OPEN R_32 CLOSE { $$ = new M8{base($3), disp($1), true}; }
| OFFSET_32 OPEN R_64 CLOSE { $$ = new M8{base($3), disp($1), false}; }
| SREG COLON OFFSET_32 OPEN R_32 CLOSE { $$ = new M8{seg($1), base($5), disp($3), true}; }
| SREG COLON OFFSET_32 OPEN R_64 CLOSE { $$ = new M8{seg($1), base($5), disp($3), false}; }
| OPEN R_32 SCALE CLOSE { $$ = new M8{index($2), $3, true}; }
| OPEN R_64 SCALE CLOSE { $$ = new M8{index($2), $3, false}; }
| SREG COLON OPEN R_32 SCALE CLOSE { $$ = new M8{seg($1), index($4), $5, true}; }
| SREG COLON OPEN R_64 SCALE CLOSE { $$ = new M8{seg($1), index($4), $5, false}; }
| OFFSET_32 OPEN R_32 SCALE CLOSE { $$ = new M8{index($3), $4, disp($1), true}; }
| OFFSET_32 OPEN R_64 SCALE CLOSE { $$ = new M8{index($3), $4, disp($1), false}; }
| SREG COLON OFFSET_32 OPEN R_32 SCALE CLOSE { $$ = new M8{seg($1), index($5), $6, disp($3), true}; }
| SREG COLON OFFSET_32 OPEN R_64 SCALE CLOSE { $$ = new M8{seg($1), index($5), $6, disp($3), false}; }
| OPEN R_32 COMMA R_32 SCALE CLOSE { $$ = new M8{base($2), index($4), $5, true}; }
| OPEN R_64 COMMA R_64 SCALE CLOSE { $$ = new M8{base($2), index($4), $5, false}; }
| SREG COLON OPEN R_32 COMMA R_32 SCALE CLOSE { $$ = new M8{seg($1), base($4), index($6), $7, true}; }
| SREG COLON OPEN R_64 COMMA R_64 SCALE CLOSE { $$ = new M8{seg($1), base($4), index($6), $7, false}; }
| OFFSET_32 OPEN R_32 COMMA R_32 SCALE CLOSE { $$ = new M8{base($3), index($5), $6, disp($1), true}; }
| OFFSET_32 OPEN R_64 COMMA R_64 SCALE CLOSE { $$ = new M8{base($3), index($5), $6, disp($1), false}; }
| SREG COLON OFFSET_32 OPEN R_32 COMMA R_32 SCALE CLOSE { $$ = new M8{seg($1), base($5), index($7), $8, disp($3), true}; }
| SREG COLON OFFSET_32 OPEN R_64 COMMA R_64 SCALE CLOSE { $$ = new M8{seg($1), base($5), index($7), $8, disp($3), false}; }

MOFFS : 
  OFFSET_64 { $$ = new Moffs8{offset($1)}; }
| SREG COLON OFFSET_64 { $$ = new Moffs8{seg($1), offset($3)}; }

REL_8 : OFFSET_8 { $$ = new Rel8{0}; delete $1; }
REL_32 : OFFSET_32 { $$ = new Rel32{0}; delete $1; }

blank : /* empty */ | blank ENDL { }

code : blank instrs blank { code.assign($2->begin(), $2->end()); delete $2; }
		 ;

instrs : instr { 
  $$ = new vector<Instruction>(); 
	$$->push_back(*$1); 
	delete $1; 
} 
| instrs instr { 
	$1->push_back(*$2); 
	delete($2); 
}
;

instr : LABEL COLON ENDL {
  $$ = new Instruction{Opcode::LABEL_DEFN, {$1}};
}
| OPCODE operands ENDL {
	$$ = make_instr(is, *$1, *$2);
	delete $1;
	delete $2;
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
;

operand : 
	HINT |
  PREF_66 | PREF_REX_W | FAR |
	LABEL |
	ZERO | ONE | THREE | IMM_8 | IMM_16 | IMM_32 | IMM_64 |
	OFFSET_8 | OFFSET_32 | OFFSET_64 |
	MM |
	AL | CL | RL | RH | RB | AX | DX | R_16 | EAX | R_32 | RAX | R_64 |
	SREG | FS | GS |
	ST_0 | ST |
	XMM_0 | XMM |
	YMM |
	M | MOFFS |
	REL_8 | REL_32 {
	$$ = $1;
}

%% 
