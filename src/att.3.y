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
