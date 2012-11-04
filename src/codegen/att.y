%{

#include <array>
#include <map>
#include <string>
#include <tuple>
#include <vector>
#include <array>

#include "src/code/code.h"
#include "src/code/imm.h"
#include "src/code/instruction.h"
#include "src/code/label.h"
#include "src/code/opcode.h"
#include "src/code/m.h"
#include "src/code/mm.h"
#include "src/code/r.h"
#include "src/code/scale.h"
#include "src/code/st.h"
#include "src/code/xmm.h"
#include "src/code/ymm.h"

using namespace std;
using namespace x64;

extern int yylex();
extern int yy_line_number;

void yyerror(std::istream& is, x64::Code& code, const char* s) { 
	is.setstate(std::ios::failbit); 
    cerr << "ERROR (line " << yy_line_number << "): " << s << endl;

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

    string bw2string(BitWidth bw) {
        switch(bw) {
            case LOW:
            case HIGH:
                return "8";
            case WORD:
                return "16";
            case DOUBLE:
                return "32";
            case QUAD:
                return "64";
            case OCT:
                return "128";
            case BIT_WIDTH_NULL:
                return "null";
            default:
                return "???";
        }
    }

    string type2string(Type t) {
        switch(t) {
            case ADDR:
                return "address";
            case FP_REG:
                return "fp reg";
            case GP_REG:
                return "gp reg";
            case IMM:
                return "immediate";
            case LABEL:
                return "label";
            case MMX_REG:
                return "mmx reg";
            case RAX_ONLY:  
                return "rax";
            case RCX_ONLY:
                return "rcx";
            case ST0_ONLY:
                return "st0";
            case TYPE_NULL:
                return "null";
            default:
                return "???";
        }
    }

    void show_line() {
    
    }

    Operand truncate_immediate(Operand o, BitWidth bw) {

        // This removes sign extension (I think).
        // It should give us the invariant that we use exactly BitWidth bits.
        if ( bw == LOW )
            o &= 0x00000000000000ff;
        else if ( bw == WORD)
            o &= 0x000000000000ffff;
        else if ( bw == DOUBLE)
            o &= 0x00000000ffffffff;
        return o;
    }

    Instruction* build_instr(istream& is, const string& opcode, 
            vector<OperandInfo>& operand_info) {
        Instruction* instr = new Instruction();

        // ***** PHASE 0: book keeping *****

        //find operand # of any immediate
        int immediate_index = -1;
        int len = operand_info.size();
        for(int i = 0;i<len;i++)
            if(operand_info[i].type == IMM)
                immediate_index = i;
            else if(operand_info[i].type == TYPE_NULL) {
                len = i;
                break;
            }

        #ifndef NDEBUG
        /*
        cerr << "Looking for a signature like this:" << endl;
        for(int i = 0; i< len;i ++) {
            cerr << "[type: " << type2string(operand_info[i].type);
            cerr << " width: " << bw2string(operand_info[i].width);
            cerr << " ]" << endl;
        }
        */
        #endif


        // ***** PHASE 1: Find all rows in spreadsheet that could plausibly work *****

        const auto opcode_sigs = signatures_[opcode];
        //opcode_sigs has a list of all signatures for the opcode

        auto options = vector<tuple<array<Type,3>,array<BitWidth,3>,OpcodeVal>>();
        //in options, we'll get only the ones with the right arguments

        for(const auto& sig : opcode_sigs) {
            bool accept = true;

        
            #ifndef NDEBUG
            /*
            cerr << "\nNew Signature attempt!" << endl;
            for(int i = 0; i < len; i++) {
                cerr << "[";
                cerr << "type: " << type2string(get<0>(sig)[i]);
                cerr << " width: " << bw2string(get<1>(sig)[i]);
                cerr << " ]";
            }
            cerr << "\n";
            */
            #endif

            for(int i = 0;i<len;i++) {

                //ensure that the argument types match
                //the chosen signature must be more general than 
                //the one we're looking for in operands_info
                if(operand_info[i].type == GP_REG) {

                    //user provided rax => spreadsheet provides GP_REG or RAX
                    if( operand_info[i].val == rax ) {

                        if ( ( get<0>(sig)[i] != RAX_ONLY ) &&
                             ( get<0>(sig)[i] != GP_REG   ) ) {
                            accept = false;
                            break;
                        }

                    //user provided rcx => spreadsheet provides GP_REG or RCX
                    } else if (operand_info[i].val == rcx ) {

                        if ( ( get<0>(sig)[i] != RCX_ONLY ) &&
                             ( get<0>(sig)[i] != GP_REG   ) ) {
                            accept = false;
                            break;
                        }

                    } else if ( get<0>(sig)[i] != GP_REG ) {
                        accept = false;
                        break;
                    }

                } else if (operand_info[i].type == FP_REG) {

                    if( operand_info[i].val == st0 ) {
                      
                        if ( ( get<0>(sig)[i] != ST0_ONLY ) &&
                             ( get<0>(sig)[i] != FP_REG   ) ) {
                            accept = false;
                            break;
                        }

                    } else if ( get<0>(sig)[i] != FP_REG ) {
                        
                        accept = false;
                        break;

                    }

                } else if (operand_info[i].type != get<0>(sig)[i] ) {

                    accept = false;
                    break;

                }
                
            
                //check that the widths are OK
                if(operand_info[i].type == IMM) {
                    //for immediates, check that we can fit the value correctly
                    //we want value in key to be less than or equal to a valid signature
                    //if( ! (operand_info[i].width <= get<1>(sig)[i]) ) {
                    //    accept=false;
                    //    break;
                    //}

                } else {
                    //otherwise, make sure widths match
                    //but ignore the width for memory.
                    if( operand_info[i].type  != ADDR &&
                        operand_info[i].width != get<1>(sig)[i]) {
                        accept = false;
                        break;
                    }
                }
            }


            if(accept) {
                #ifndef NDEBUG
                /*
                cerr << "accepted possible:";
                for(int i = 0; i < 3; i++) {
                    cerr << "[";
                    cerr << "type: " << type2string(get<0>(sig)[i]);
                    cerr << " width: " << bw2string(get<1>(sig)[i]);
                    cerr << " ]";
                }
                cerr << "\n";
                */
                #endif

                options.push_back(sig);
            }
        }

        // ***** PHASE 2: Find the rows that we want.  *****
        //  This means the rows are
        //  (1) are specific, e.g. using EAX variants instead of general purpose
        //  (2) have small immediate fields whenever possible.

        //when we loop through options, we'll use these to figure out the right one to pick.
        int max_score  = -10000000;
        int max_score_index = -1;
        int index = 0;

        for(const auto &opt : options) {


            //PART (a): Compute Score
            int score = 0;

            //the smaller the width, the better
            if (immediate_index != -1) {
                if(operand_info[immediate_index].width <= get<1>(opt)[immediate_index])
                    score += 100*(20-get<1>(opt)[immediate_index]);
                else
                    score -= 1000*(20 - get<1>(opt)[immediate_index]) ;
            }


            //you get a few points for having a more specific register
            for(int i = 0; i < len; i++ )
                if(get<0>(opt)[i] == RAX_ONLY ||
                   get<0>(opt)[i] == RCX_ONLY ||
                   get<0>(opt)[i] == ST0_ONLY)
                   score += 5;



            //PART (b): Update Variables
            if(score > max_score) {
                max_score = score;
                max_score_index = index;
            }

            index++;

        }



        // ***** PHASE 3: Cleanup.  Resize immediates, print debug info, etc.  *****

        //get the opcode
        bool parse_success = (max_score_index >= 0);
        OpcodeVal parsed_opcode = NOP;

        if(parse_success) {
            parsed_opcode = get<2>(options[max_score_index]);

            if(immediate_index != -1) {
                int i = immediate_index;
                operand_info[i].width = get<1>(options[max_score_index])[i];
                operand_info[i].val = truncate_immediate(operand_info[i].val, 
                        operand_info[i].width);
            }

        }

        //resize immediate
        // Trouble with parsing?  Try uncommenting this to see signatures
        //Note: this code needs to be revised to not use key.
        /*
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
            cerr << dec << (parsed_opcode >> 50);
        else
            cerr << "???";
        cerr << endl;
        */

        vector<Operand> ops;

        if ( !parse_success ) {
            *instr = Instruction(NOP, ops.begin(), ops.end());
            x64::Code dumb = x64::Code();
            yyerror(is, dumb, "Could not parse instruction. Did you use the right memonic for these operands?");
            cerr << "memonic: " << opcode << endl;
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

		// Memory Displacement Methods:

		// Does this value fit into 32 bits?
    inline bool is_valid_disp(Operand d) {
			return (int64_t) d == (int32_t) d;
    }

    void invalid_disp(istream& is, Operand d) {
        x64::Code dumb = x64::Code();
        yyerror(is, dumb, "Bad displacement for memory argument.");
    }

    void scale_for_no_index(istream& is) {
        x64::Code dumb = x64::Code();
        yyerror(is, dumb, "Bad scale for no index.");
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
                 | operand COMMA operand {
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
          ATT_OFFSET | ATT_SCALE  | ATT_XMM_REG | mem
        ;

mem : OPEN ATT_GP_REG CLOSE { //(%rax)
				$$ = new OperandInfo($2->width == DOUBLE ?
						M(R32($2->val)) :
						M(R64($2->val)), 
						ADDR, 
						$2->width); 
				delete $2; 
			}

    | ATT_OFFSET OPEN ATT_GP_REG CLOSE { //0x10(%rax)
			if ( !is_valid_disp($1->val) ) 
                invalid_disp(is,$1->val);

			$$ = new OperandInfo($3->width == DOUBLE ?
					M(R32($3->val), Imm32($1->val)) :
					M(R64($3->val), Imm32($1->val)),
					ADDR, 
					$3->width); 
			delete $1; 
			delete $3; 
		} 
    | OPEN ATT_GP_REG COMMA COMMA ATT_SCALE CLOSE { //(%rax,,1)
                if ( $5->val != times_1 ) 
                    scale_for_no_index(is);

				$$ = new OperandInfo($2->width == DOUBLE ?
						M(R32($2->val)) :
						M(R64($2->val)), 
						ADDR, 
						$2->width); 
				delete $2; 
			}
    | OPEN ATT_GP_REG COMMA ATT_SCALE CLOSE { //(%rax,1)
        if ( $4->val != times_1 ) 
          scale_for_no_index(is);

				$$ = new OperandInfo($2->width == DOUBLE ?
						M(R32($2->val)) :
						M(R64($2->val)), 
						ADDR, 
						$2->width); 
				delete $2; 
			}
    | ATT_OFFSET OPEN ATT_GP_REG COMMA COMMA ATT_SCALE CLOSE { //0x10(%rax,,1)
			if ( !is_valid_disp($1->val) ) 
                invalid_disp(is, $1->val);
            if ( $6->val != times_1 ) 
                scale_for_no_index(is);

			$$ = new OperandInfo($3->width == DOUBLE ?
					M(R32($3->val), Imm32($1->val)) :
					M(R64($3->val), Imm32($1->val)),
					ADDR, 
					$3->width); 
			delete $1; 
			delete $3; 
		} 
    | OPEN ATT_GP_REG COMMA ATT_GP_REG CLOSE { //(%rax,%rax)
			$$ = new OperandInfo($2->width == DOUBLE ?
					M(R32($2->val), R32($4->val), Scale(times_1)) :
					M(R64($2->val), R64($4->val), Scale(times_1)),
					ADDR, 
					$2->width); 
			delete $2; 
			delete $4; 
		}
    | ATT_OFFSET OPEN ATT_GP_REG COMMA ATT_GP_REG CLOSE { //0x10(%rax, %rax)
			if ( !is_valid_disp($1->val) )
                invalid_disp(is,$1->val);

			$$ = new OperandInfo($3->width == DOUBLE ?
					M(R32($3->val), R32($5->val), Scale(times_1), Imm32($1->val)) :
					M(R64($3->val), R64($5->val), Scale(times_1), Imm32($1->val)), 
					ADDR, 
					$3->width); 
			delete $1;
			delete $3; 
			delete $5; 
		}
    | OPEN ATT_GP_REG COMMA ATT_GP_REG COMMA ATT_SCALE CLOSE { //(%rax, %rax, 2)
			$$ = new OperandInfo($2->width == DOUBLE ?
					M(R32($2->val), R32($4->val), Scale($6->val)) :
					M(R64($2->val), R64($4->val), Scale($6->val)), 
					ADDR, 
					$2->width); 
			delete $2; 
			delete $4; 
			delete $6; 
		}
    | ATT_OFFSET OPEN ATT_GP_REG COMMA ATT_GP_REG COMMA ATT_SCALE CLOSE {  //0x10(%rax,%rax,2)
			if ( !is_valid_disp($1->val) ) 
                invalid_disp(is,$1->val);

			$$ = new OperandInfo($3->width == DOUBLE ?
						M(R32($3->val), R32($5->val), Scale($7->val), Imm32($1->val)) :
						M(R64($3->val), R64($5->val), Scale($7->val), Imm32($1->val)), 
					ADDR, 
					$3->width); 
			delete $1; 
			delete $3; 
			delete $5; 
			delete $7; 
		}
    | OPEN COMMA ATT_GP_REG COMMA ATT_SCALE CLOSE { //(,%rax,2)
            $$ = new OperandInfo($3->width == DOUBLE ?
                        M(R32($3->val), Scale($5->val)) :
                        M(R64($3->val), Scale($5->val)),
                    ADDR,
                    $3->width);
            delete $3;
            delete $5;
    }
    | ATT_OFFSET OPEN COMMA ATT_GP_REG COMMA ATT_SCALE CLOSE { //0x10(,%rax,2)
            $$ = new OperandInfo($4->width == DOUBLE ?
                        M(R32($4->val), Scale($6->val), Imm32($1->val)) :
                        M(R64($4->val), Scale($6->val), Imm32($1->val)),
                    ADDR,
                    $4->width);
            delete $1;
            delete $4;
            delete $6;
    }
    | OPEN COMMA ATT_GP_REG CLOSE { //(,%rax)
            $$ = new OperandInfo($3->width == DOUBLE ?
                        M(R32($3->val), Scale(times_1)) :
                        M(R64($3->val), Scale(times_1)),
                    ADDR,
                    $3->width);
            delete $3;
    }
    | ATT_OFFSET OPEN COMMA ATT_GP_REG CLOSE { //0x10(,%rax)
            $$ = new OperandInfo($4->width == DOUBLE ?
                        M(R32($4->val), Scale(times_1), Imm32($1->val)) :
                        M(R64($4->val), Scale(times_1), Imm32($1->val)),
                    ADDR,
                    $4->width);
            delete $1;
            delete $4;
    }
    ;

%% 
