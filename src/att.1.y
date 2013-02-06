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

#include <vector>

#include "src/code.h"
#include "src/instruction.h"
#include "src/m.h"
#include "src/moffs.h"
#include "src/opcode.h"
#include "src/rel.h"

using namespace std;
using namespace x64asm;

extern int yylex();
extern int yy_line_number;

void yyerror(std::istream& is, x64asm::Code& code, const char* s) { 
	is.setstate(std::ios::failbit); 
	cerr << "Error on line " << yy_line_number << ": "  << s << endl;
}

const M* m(const Operand* o) { return dynamic_cast<const M*>(o); }
const Moffs* moffs(const Operand* o) { return dynamic_cast<const Moffs*>(o); }
const Sreg* seg(const Operand* o) { return dynamic_cast<const Sreg*>(o); }
const AddrR* base(const Operand* o) { return dynamic_cast<const AddrR*>(o); }
const AddrR* index(const Operand* o) { return dynamic_cast<const AddrR*>(o); }
const Imm32* disp(const Operand* o) { return dynamic_cast<const Imm32*>(o); }
const Imm64* offset(const Operand* o) { return dynamic_cast<const Imm64*>(o); }

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
	const x64asm::Operand* operand;
	x64asm::Opcode opcode;
	x64asm::Instruction* instr;
  std::vector<x64asm::Instruction>* instrs;
}

/** Simple tokens. */

%token <int> COMMA
%token <int> COLON
%token <int> OPEN
%token <int> CLOSE
%token <int> ENDL

/** Atomic operands. */

%token <scale> SCALE

%token <operand> HINT
%token <operand> PREF_66
%token <operand> PREF_REX_W
%token <operand> FAR
%token <operand> LABEL

%token <operand> ZERO 
%token <operand> ONE 
%token <operand> THREE 
%token <operand> AN_IMM_8 
%token <operand> AN_IMM_16
%token <operand> AN_IMM_32 
%token <operand> AN_IMM_64
%token <operand> AN_OFFSET_8
%token <operand> AN_OFFSET_32
%token <operand> AN_OFFSET_64
%token <operand> MM
%token <operand> AL
%token <operand> CL
%token <operand> AN_RL
%token <operand> RH
%token <operand> RB
%token <operand> AX
%token <operand> DX
%token <operand> AN_R_16
%token <operand> EAX
%token <operand> AN_R_32
%token <operand> RAX
%token <operand> AN_R_64
%token <operand> AN_SREG
%token <operand> FS
%token <operand> GS
%token <operand> ST_0
%token <operand> AN_ST
%token <operand> XMM_0
%token <operand> AN_XMM
%token <operand> YMM

/** Compound operands. */

%type <operand> MEM
%type <operand> MOFFS

%type <operand> FAR_PTR_16_16 
%type <operand> FAR_PTR_16_32 
%type <operand> FAR_PTR_16_64 
%type <operand> M_8 
%type <operand> M_16
%type <operand> M_32 
%type <operand> M_64 
%type <operand> M_128 
%type <operand> M_256 
%type <operand> M_32_FP 
%type <operand> M_64_FP 
%type <operand> M_80_FP 
%type <operand> M_16_INT 
%type <operand> M_32_INT 
%type <operand> M_64_INT 
%type <operand> M_80_BCD 
%type <operand> M_2_BYTE 
%type <operand> M_28_BYTE 
%type <operand> M_108_BYTE 
%type <operand> M_512_BYTE 
%type <operand> MOFFS_8 
%type <operand> MOFFS_16 
%type <operand> MOFFS_32 
%type <operand> MOFFS_64 
%type <operand> REL_8 
%type <operand> REL_32 

/** Atomic operands with non-trivial type relationships. */

%type <operand> IMM_8
%type <operand> IMM_16
%type <operand> IMM_32
%type <operand> IMM_64
%type <operand> OFFSET_8
%type <operand> OFFSET_32
%type <operand> OFFSET_64
%type <operand> RL
%type <operand> R_16
%type <operand> R_32
%type <operand> R_64
%type <operand> SREG
%type <operand> ST
%type <operand> XMM

/** Opcode tokens */


