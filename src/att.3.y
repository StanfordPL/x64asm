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

%type <instr> instr
%type <instrs> instrs

%locations
%error-verbose

%parse-param { std::istream& is }
%parse-param { x64asm::Code& code }

%start code

%%

MEM : 
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

FAR_PTR_16_16 : MEM { $$ = make_mem<FarPtr1616>(m($1)); delete $1; }
FAR_PTR_16_32 : MEM { $$ = make_mem<FarPtr1632>(m($1)); delete $1; }
FAR_PTR_16_64 : MEM { $$ = make_mem<FarPtr1664>(m($1)); delete $1; }
M_8           : MEM { $$ = make_mem<M8>(m($1)); delete $1; }
M_16          : MEM { $$ = make_mem<M16>(m($1)); delete $1; }
M_32          : MEM { $$ = make_mem<M32>(m($1)); delete $1; }
M_64          : MEM { $$ = make_mem<M64>(m($1)); delete $1; }
M_128         : MEM { $$ = make_mem<M128>(m($1)); delete $1; }
M_256         : MEM { $$ = make_mem<M256>(m($1)); delete $1; }
M_32_FP       : MEM { $$ = make_mem<M32Fp>(m($1)); delete $1; }
M_64_FP       : MEM { $$ = make_mem<M64Fp>(m($1)); delete $1; }
M_80_FP       : MEM { $$ = make_mem<M80Fp>(m($1)); delete $1; }
M_16_INT      : MEM { $$ = make_mem<M16Int>(m($1)); delete $1; }
M_32_INT      : MEM { $$ = make_mem<M32Int>(m($1)); delete $1; }
M_64_INT      : MEM { $$ = make_mem<M64Int>(m($1)); delete $1; }
M_80_BCD      : MEM { $$ = make_mem<M80Bcd>(m($1)); delete $1; }
M_2_BYTE      : MEM { $$ = make_mem<M2Byte>(m($1)); delete $1; }
M_28_BYTE     : MEM { $$ = make_mem<M28Byte>(m($1)); delete $1; }
M_108_BYTE    : MEM { $$ = make_mem<M108Byte>(m($1)); delete $1; }
M_512_BYTE    : MEM { $$ = make_mem<M512Byte>(m($1)); delete $1; }

MOFFS : 
  OFFSET_64 { $$ = new Moffs8{offset($1)}; }
| SREG COLON OFFSET_64 { $$ = new Moffs8{seg($1), offset($3)}; }

MOFFS_8  : MOFFS { $$ = make_moffs<Moffs8>(moffs($1)); delete $1; }
MOFFS_16 : MOFFS { $$ = make_moffs<Moffs16>(moffs($1)); delete $1; }
MOFFS_32 : MOFFS { $$ = make_moffs<Moffs32>(moffs($1)); delete $1; }
MOFFS_64 : MOFFS { $$ = make_moffs<Moffs64>(moffs($1)); delete $1; }

REL_8 : OFFSET_8 { $$ = new Rel8{0}; delete $1; }
REL_32 : OFFSET_32 { $$ = new Rel32{0}; delete $1; }

IMM_64    : AN_IMM_64 | AN_IMM_32 | AN_IMM_16 | AN_IMM_8 | ZERO | ONE | THREE ;
IMM_32    : AN_IMM_32 | AN_IMM_16 | AN_IMM_8 | ZERO | ONE | THREE ;
IMM_16    : AN_IMM_16 | AN_IMM_8 | ZERO | ONE | THREE ;
IMM_8     : AN_IMM_8 | ZERO | ONE | THREE ;
OFFSET_64 : AN_OFFSET_64 | AN_OFFSET_32 | AN_OFFSET_8 ;
OFFSET_32 : AN_OFFSET_32 | AN_OFFSET_8 ;
OFFSET_8  : AN_OFFSET_8 ;
RL        : AL | CL | AN_RL ;
R_16      : AX | DX | AN_R_16 ;
R_32      : EAX | AN_R_32 ;
R_64      : RAX | AN_R_64 ;
SREG      : FS | GS | AN_SREG ;
ST        : ST_0 | AN_ST ;
XMM       : XMM_0 | AN_XMM ;

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
