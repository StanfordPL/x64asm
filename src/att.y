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

%token <opcode>  OPCODE

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

%token <opcode> ADCB
%token <opcode> ADCW

%type <instr> instr
%type <instrs> instrs

%locations
%error-verbose

%parse-param { std::istream& is }
%parse-param { x64asm::Code& code }

%start code

%%

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

instr : ADCB IMM_8 COMMA AL ENDL {
        $$ = new Instruction{Opcode::ADC_AL_IMM8, {$4, $2}};
			}
      | ADCW IMM_16 COMMA AX ENDL { 
				$$ = new Instruction{Opcode::ADC_AX_IMM16, {$4, $2}};
			} 
      ;

%% 
