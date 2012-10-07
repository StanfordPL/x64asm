%{

#include <map>
#include <string>
#include <tuple>
#include <vector>

#include "src/code/code.h"
#include "src/code/gp_reg.h"
#include "src/code/imm.h"
#include "src/code/instruction.h"
#include "src/code/label.h"
#include "src/code/opcode.h"
#include "src/code/scale.h"
#include "src/code/xmm_reg.h"

using namespace std;
using namespace x64;

extern int yylex();
void yyerror(std::istream& is, x64::Code& code, const char* s) { 
	is.setstate(std::ios::failbit); 
	printf("ERROR: %s\n", s); 
}

%}

%code requires {

#include "src/code/operand.h"

#ifndef ATT_OPERAND_INFO
#define ATT_OPERAND_INFO

struct OperandInfo {
	OperandInfo(x64::Operand v, x64::Type t, x64::BitWidth w) 
			: val(v), type(t), width(w) {
	}

	x64::Operand val;
	x64::Type type;
	x64::BitWidth width;
};

#endif

}

%code {

// map<tuple<string, array, array>, Opcode> signatures_ {{ ... }}
#include "src/gen/opcode.sigs"

Instruction* build_instr(istream& is, const string& opcode, 
		                     const vector<OperandInfo>& operand_info) {
	Instruction* instr = new Instruction();
	auto key = make_tuple(opcode, array<Type, 3>(), array<BitWidth, 3>());

	size_t i = 0;
	for ( size_t ie = operand_info.size(); i < ie; ++i ) {
		get<1>(key)[i] = operand_info[i].type;
		get<2>(key)[i] = operand_info[i].width;
	}
	for ( ; i < 3; ++i ) {
		get<1>(key)[i] = TYPE_NULL;
		get<2>(key)[i] = BIT_WIDTH_NULL;
	}

	// A nasty bit of context sensitivity with the SHL/SHR class of instructions
	// CL in the right place is a RCX_ONLY (we can't check this until now)
	// Ditto for the arithmetic ops and AL,AX,EAX,RAX and RAX_ONLY

	const auto len = opcode.length() > 3 ? 3 : opcode.length();
	const auto base = opcode.substr(0, len);
	if ( base == "sal" || base == "sar" || base == "shl" || base == "shr" )
		if ( operand_info.size() == 2 && operand_info[1].type == GP_REG ) {
			if ( operand_info[1].val != rcx || operand_info[1].width != LOW )
				is.setstate(std::ios::failbit);
			get<1>(key)[1] = RCX_ONLY;
		}

	if ( base == "adc" || base == "add" || base == "and" || base == "cmp" ||
			 base == "or"  || base == "sbb" || base == "sub" || base == "xor" )
		if ( operand_info[0].type == GP_REG && operand_info[0].val == rax && 
				 operand_info[1].type == IMM )
			if ((operand_info[0].width == QUAD && operand_info[1].width == DOUBLE) || 
					(operand_info[0].width == operand_info[1].width) ) 
				get<1>(key)[0] = RAX_ONLY;

	// Similar issue related to floating point instructions which require st0

	if ( operand_info.size() >= 1 && operand_info[0].type == FP_REG && operand_info[0].val == st0 )
		get<1>(key)[0] = ST0_ONLY;
	if ( operand_info.size() >= 2 && operand_info[1].type == FP_REG && operand_info[1].val == st0 )
		get<1>(key)[1] = ST0_ONLY;

	vector<Operand> ops;
	const auto itr = signatures_.find(key);

	// Trouble with parsing?  Try uncommenting this to see signatures
	cerr << opcode << "\t type [ ";
	for ( const auto& i : get<1>(key) )
		cerr << i << " ";
	cerr << "] width [ ";
	for ( const auto& i : get<2>(key) )
		cerr << i << " ";
	cerr << "] val [ ";
	for ( const auto& i : operand_info )
		cerr << hex << i.val << " ";
	cerr << "] ---> ";
	if ( itr != signatures_.end() )
		cerr << dec << itr->second;
	else
		cerr << "???";
	cerr << endl;

	if ( itr == signatures_.end() ) {
		instr->set_all(NOP, ops.begin(), ops.end());
		is.setstate(std::ios::failbit);
	}
	else {
 		for ( const auto& i : operand_info )
			ops.push_back(i.val);
		instr->set_all(itr->second, ops.begin(), ops.end());
	}

	return instr;
}

Instruction* build_label(const OperandInfo& label) {
	Instruction* instr = new Instruction();

	vector<Operand> ops {{ label.val }};
	instr->set_all(LABEL_DEFN_64L, ops.begin(), ops.end());

	return instr;
}

}

%union {
	std::string* str;
	OperandInfo* operand;
	std::vector<OperandInfo>* operands;
	Instruction* instr;
	std::vector<Instruction>* instrs;
}

%token <int> COMMA
%token <int> COLON
%token <int> OPEN
%token <int> CLOSE
%token <int> ENDL

%token <operand> ATT_FP_REG 
%token <operand> ATT_GP_REG 
%token <operand> ATT_IMM 
%token <operand> ATT_LABEL 
%token <operand> ATT_MMX_REG
%token <operand> ATT_SCALE 
%token <operand> ATT_XMM_REG

%token <str>  ATT_OPCODE

%type <operand> operand
%type <operands> operands
%type <instr> instr
%type <instrs> instrs

%type <operand> mem

%locations
%error-verbose

%parse-param { std::istream& is }
%parse-param { x64::Code& code }

%start code

%%

code : instrs { code.assign($1->begin(), $1->end()); delete $1; }
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

instr : ATT_OPCODE operands ENDL { 
				$$ = build_instr(is, *$1, *$2); 
				delete $1; 
				delete $2; 
			}
      | ATT_LABEL COLON ENDL {
				$$ = build_label(*$1); 
				delete $1; 
			} 
      ;

operands : /* empty */ { 
			 		 $$ = new vector<OperandInfo>(); 
				 }
         | operand { 
					 $$ = new vector<OperandInfo>(); 
					 $$->push_back(*$1); 
					 delete $1; 
				 }
				 | mem { 
					 $$ = new vector<OperandInfo>(); 
					 $$->push_back(*$1); 
					 delete $1; 
				 }
				 | operand COMMA operand { 
					 $$ = new vector<OperandInfo>(); 
					 $$->push_back(*$3); 
					 $$->push_back(*$1); 
					 delete $1; 
					 delete $3; 
				 } 
				 | mem COMMA operand { 
					 $$ = new vector<OperandInfo>(); 
					 $$->push_back(*$3); 
					 $$->push_back(*$1); 
					 delete $1; 
					 delete $3; 
				 } 
				 | operand COMMA mem { 
					 $$ = new vector<OperandInfo>(); 
					 $$->push_back(*$3); 
					 $$->push_back(*$1); 
					 delete $1; 
					 delete $3; 
				 } 
				 | operand COMMA operand COMMA operand { 
					 $$ = new vector<OperandInfo>(); 
					 $$->push_back(*$5); 
					 $$->push_back(*$3); 
					 $$->push_back(*$1); 
					 delete $1; 
					 delete $3; 
					 delete $5; 
				 } 
				 ;

operand : ATT_FP_REG | ATT_GP_REG | ATT_IMM | ATT_LABEL | ATT_MMX_REG |
          ATT_SCALE  | ATT_XMM_REG
        ;

mem : OPEN ATT_GP_REG CLOSE { 
				$$ = new OperandInfo(Addr(GpReg($2->val)), 
						ADDR, 
						$2->width); 
				delete $2; 
			}
    | ATT_IMM OPEN ATT_GP_REG CLOSE { 
			$$ = new OperandInfo(Addr(GpReg($3->val), Imm($1->val)), 
					ADDR, 
					$3->width); 
			delete $1; 
			delete $3; 
		} 
    | OPEN ATT_GP_REG COMMA ATT_GP_REG CLOSE { 
			$$ = new OperandInfo(
					Addr(GpReg($2->val), GpReg($4->val)), 
					ADDR, 
					$2->width); 
			delete $2; 
			delete $4; 
		}
    | OPEN ATT_GP_REG COMMA ATT_GP_REG COMMA ATT_SCALE CLOSE { 
			$$ = new OperandInfo(
					Addr(GpReg($2->val), GpReg($4->val), (ScaleVal) $6->val), 
					ADDR, 
					$2->width); 
			delete $2; 
			delete $4; 
			delete $6; 
		}
    | ATT_IMM OPEN ATT_GP_REG COMMA ATT_GP_REG CLOSE { 
			$$ = new OperandInfo(
					Addr(GpReg($3->val), GpReg($5->val), Imm($1->val)), 
					ADDR, 
					$3->width); 
			delete $1;
			delete $3; 
			delete $5; 
		}
    | ATT_IMM OPEN ATT_GP_REG COMMA ATT_GP_REG COMMA ATT_SCALE CLOSE { 
			$$ = new OperandInfo(
					Addr(GpReg($3->val), GpReg($5->val), (ScaleVal) $7->val, Imm($1->val)), 
					ADDR, 
					$3->width); 
			delete $1; 
			delete $3; 
			delete $5; 
			delete $7; 
		}
    ;

%% 
