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
#include "src/opcode.h"

using namespace std;
using namespace x64asm;

extern int yylex();
extern int yy_line_number;

void yyerror(std::istream& is, x64asm::Code& code, const char* s) { 
	is.setstate(std::ios::failbit); 
	cerr << "Error on line " << yy_line_number << ": "  << s << endl;
}

%}

%code requires {
	#include "src/code.h"
	#include "src/operand.h"
  #include "src/instruction.h"
}

%union {
	x64asm::Opcode opcode;
	const x64asm::Operand* operand;
	x64asm::Instruction* instr;
  std::vector<x64asm::Instruction>* instrs;
}

%token <int> COMMA
%token <int> COLON
%token <int> OPEN
%token <int> CLOSE
%token <int> ENDL

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
%token <operand> CX
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
