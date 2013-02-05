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

%type <operand> HINT 
%type <operand> LABEL 
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
%type <operand> M_14_BYTE 
%type <operand> M_28_BYTE 
%type <operand> M_94_BYTE 
%type <operand> M_108_BYTE 
%type <operand> M_512_BYTE 
%type <operand> MOFFS_8 
%type <operand> MOFFS_16 
%type <operand> MOFFS_32 
%type <operand> MOFFS_64 
%type <operand> REL_8 
%type <operand> REL_32 

%type <operand> IMM_8
%type <operand> IMM_16
%type <operand> IMM_32
%type <operand> IMM_64
%type <operand> RL
%type <operand> R_16
%type <operand> R_32
%type <operand> R_64
%type <operand> SREG
%type <operand> ST
%type <operand> XMM

%type <instr> instr
%type <instrs> instrs

%locations
%error-verbose

%parse-param { std::istream& is }
%parse-param { x64asm::Code& code }

%start code

%%

HINT : RAX { $$ = new Constants::rax{}; }
LABEL : RAX { $$ = new Constants::rax{}; }
FAR_PTR_16_16 : RAX { $$ = new Constants::rax{}; }
FAR_PTR_16_32 : RAX { $$ = new Constants::rax{}; }
FAR_PTR_16_64 : RAX { $$ = new Constants::rax{}; }
M_8 : RAX { $$ = new Constants::rax{}; }
M_16 : RAX { $$ = new Constants::rax{}; }
M_32 : RAX { $$ = new Constants::rax{}; }
M_64 : RAX { $$ = new Constants::rax{}; }
M_128 : RAX { $$ = new Constants::rax{}; }
M_256 : RAX { $$ = new Constants::rax{}; }
M_32_FP : RAX { $$ = new Constants::rax{}; }
M_64_FP : RAX { $$ = new Constants::rax{}; }
M_80_FP : RAX { $$ = new Constants::rax{}; }
M_16_INT : RAX { $$ = new Constants::rax{}; }
M_32_INT : RAX { $$ = new Constants::rax{}; }
M_64_INT : RAX { $$ = new Constants::rax{}; }
M_80_BCD : RAX { $$ = new Constants::rax{}; }
M_2_BYTE : RAX { $$ = new Constants::rax{}; }
M_14_BYTE : RAX { $$ = new Constants::rax{}; }
M_28_BYTE : RAX { $$ = new Constants::rax{}; }
M_94_BYTE : RAX { $$ = new Constants::rax{}; }
M_108_BYTE : RAX { $$ = new Constants::rax{}; }
M_512_BYTE : RAX { $$ = new Constants::rax{}; }
MOFFS_8 : RAX { $$ = new Constants::rax{}; }
MOFFS_16 : RAX { $$ = new Constants::rax{}; }
MOFFS_32 : RAX { $$ = new Constants::rax{}; }
MOFFS_64 : RAX { $$ = new Constants::rax{}; }
REL_8 : RAX { $$ = new Constants::rax{}; }
REL_32 : RAX { $$ = new Constants::rax{}; }

IMM_8  : ZERO   | ONE       | THREE   | AN_IMM_8 ;
IMM_16 : IMM_8  | AN_IMM_16 ;
IMM_32 : IMM_16 | AN_IMM_16 ;
IMM_64 : IMM_32 | AN_IMM_64 ;
RL     : AL     | CL        | AN_RL   ;
R_16   : AX     | DX        | AN_R_16 ;
R_32   : EAX    | AN_R_32   ;
R_64   : RAX    | AN_R_64   ;
SREG   : FS     | GS        | AN_SREG ;
ST     : ST_0   | AN_ST     ;
XMM    : XMM_0  | AN_XMM    ;

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

 /* Dummy rule -- replace this with label_defn */
instr : OPC_ADCB IMM_8 AL ENDL {
  $$ = new Instruction{Opcode::ADC_AL_IMM8, {$4, $2}};
}
