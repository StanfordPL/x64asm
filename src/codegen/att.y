%{

#include <array>
#include <map>
#include <string>
#include <tuple>
#include <vector>
#include <array>

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

#include "src/gen/opcode.sigs"

    Operand truncate_immediate(Operand o, BitWidth bw) {

        // This removes sign extension (I think).
        // It should give us the invariant that we use exactly BitWidth bits.
        if ( bw == LOW )
            o &= 0x00000000000000ff;
        else if ( bw == WORD)
            o &= 0x000000000000ffff;
        else if ( bw == DOUBLE)
            o &= 0x00000000ffffffff;
        return 0;
    }


    Instruction* build_instr(istream& is, const string& opcode, 
            vector<OperandInfo>& operand_info) {
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
        {
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
        }

        // Similar issue related to floating point instructions which require st0
        if ( operand_info.size() >= 1 && operand_info[0].type == FP_REG && operand_info[0].val == st0 )
            get<1>(key)[0] = ST0_ONLY;
        if ( operand_info.size() >= 2 && operand_info[1].type == FP_REG && operand_info[1].val == st0 )
            get<1>(key)[1] = ST0_ONLY;

        // And once again for movabs
        {
            const auto len = opcode.length() > 6 ? 6 : opcode.length();
            const auto base = opcode.substr(0, len);
            if ( base == "movabs" ) {
                if ( operand_info[0].type == GP_REG && operand_info[0].val == rax )
                    get<1>(key)[0] = RAX_ONLY;
                else if ( operand_info[1].type == GP_REG && operand_info[1].val == rax )
                    get<1>(key)[1] = RAX_ONLY;
                else
                    is.setstate(std::ios::failbit);
            }
        }

        vector<Operand> ops;
        const auto opcode_sigs = signatures_[get<0>(key)];
        //opcode_sigs has a list of all signatures for the opcode

        auto options = vector<OpcodeVal>();
        //in options, we'll get only the ones with the right arguments

        //when we loop through options, we'll use these to figure out the right one to pick.
        BitWidth min_immediate_size  = (BitWidth)65535;
        int min_immediate_index = -1;
        int index = 0;

        for(const auto& sig : opcode_sigs) {
            bool accept = true;
            for(int i = 0;i<3;i++) {

                //ensure that the argument types match
                if(get<1>(key)[i] != get<0>(sig)[i]) {
                    accept = false;

                    if(get<1>(key)[i] == IMM) {
                        //for immediates, check that we can fit the value correctly
                        if(get<2>(key)[i] < get<1>(sig)[i])
                            accept=false;

                    } else {
                        //otherwise, make sure widths match
                        if(get<2>(key)[i] != get<1>(sig)[i])
                            accept = false;
                    }
                }
            }
            if(accept) {
                //figure out which has smallest immediate value
                for(int i = 0;i<3;i++)
                    if(get<0>(sig)[i] == IMM) {
                        if(get<1>(sig)[i] < min_immediate_size) {
                            min_immediate_size = get<1>(sig)[i];
                            min_immediate_index = index;

                            //now truncate the immediate, and update the width
                            operand_info[i].width = min_immediate_size;
                            operand_info[i].val = truncate_immediate(operand_info[i].val, 
                                                                        min_immediate_size);
                        }
                    }

                options.push_back(get<2>(sig));
                index++;
            }
        }

        bool parse_success = (options.size()) > 0;
        OpcodeVal parsed_opcode = NOP;

        if(parse_success) {
            if(min_immediate_index == -1)
                parsed_opcode = options.front();
            else
                parsed_opcode = options[min_immediate_index];
        }

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
        if ( parse_success )
            cerr << dec << parsed_opcode;
        else
            cerr << "???";
        cerr << endl;

        if ( !parse_success ) {
            *instr = Instruction(NOP, ops.begin(), ops.end());
            is.setstate(std::ios::failbit);
        }
        else {
            for ( const auto& i : operand_info )
                ops.push_back(i.val);
            *instr = Instruction(parsed_opcode, ops.begin(), ops.end());
        }

        return instr;
    }

    Instruction* build_label(const OperandInfo& label) {
        Instruction* instr = new Instruction();

        vector<Operand> ops {{ label.val }};
        *instr = Instruction(LABEL_DEFN_64L, ops.begin(), ops.end());

        return instr;
    }

    bool is_valid_disp(Operand d) {
        const auto top = (d & 0xffffffff00000000) >> 32;
        return (top == 0) || (top == 0xffffffff);
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
%token <operand> ATT_OFFSET

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

blank : /* empty */ | blank ENDL { }

code : blank instrs { code.assign($2->begin(), $2->end()); delete $2; }
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

instr : ATT_OPCODE operands ENDL blank { 
				$$ = build_instr(is, *$1, *$2); 
				delete $1; 
				delete $2; 
			}
      | ATT_LABEL COLON ENDL blank {
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
          ATT_OFFSET | ATT_SCALE  | ATT_XMM_REG
        ;

mem : OPEN ATT_GP_REG CLOSE { 
				$$ = new OperandInfo(Addr(GpReg($2->val), $2->width == DOUBLE), 
						ADDR, 
						$2->width); 
				delete $2; 
			}
    | ATT_OFFSET OPEN ATT_GP_REG CLOSE { 
			if ( !is_valid_disp($1->val) )
				is.setstate(std::ios::failbit);

			$$ = new OperandInfo(Addr(GpReg($3->val), Imm($1->val), $3->width == DOUBLE), 
					ADDR, 
					$3->width); 
			delete $1; 
			delete $3; 
		} 
    | OPEN ATT_GP_REG COMMA ATT_GP_REG CLOSE { 
			$$ = new OperandInfo(
					Addr(GpReg($2->val), GpReg($4->val), $2->width == DOUBLE), 
					ADDR, 
					$2->width); 
			delete $2; 
			delete $4; 
		}
    | OPEN ATT_GP_REG COMMA ATT_GP_REG COMMA ATT_SCALE CLOSE { 
			$$ = new OperandInfo(
					Addr(GpReg($2->val), GpReg($4->val), Scale($6->val), $2->width == DOUBLE), 
					ADDR, 
					$2->width); 
			delete $2; 
			delete $4; 
			delete $6; 
		}
    | ATT_OFFSET OPEN ATT_GP_REG COMMA ATT_GP_REG CLOSE { 
			if ( !is_valid_disp($1->val) )
				is.setstate(std::ios::failbit);

			$$ = new OperandInfo(
					Addr(GpReg($3->val), GpReg($5->val), Imm($1->val), $3->width == DOUBLE), 
					ADDR, 
					$3->width); 
			delete $1;
			delete $3; 
			delete $5; 
		}
    | ATT_OFFSET OPEN ATT_GP_REG COMMA ATT_GP_REG COMMA ATT_SCALE CLOSE { 
			if ( !is_valid_disp($1->val) )
				is.setstate(std::ios::failbit);

			$$ = new OperandInfo(
					Addr(GpReg($3->val), GpReg($5->val), Scale($7->val), Imm($1->val), $3->width == DOUBLE), 
					ADDR, 
					$3->width); 
			delete $1; 
			delete $3; 
			delete $5; 
			delete $7; 
		}
    ;

%% 
