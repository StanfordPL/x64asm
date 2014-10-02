# Copyright 2103 eric schkufza

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at

#     http://www.apache.org/licenses/LICENSE-2.0

# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

##### CONSTANT DEFINITIONS

GCC=ccache g++ -Werror -Wextra -pedantic -std=c++11

INC=-I./
		
OBJ=src/assembler.o \
		src/code.o \
		src/constants.o \
		src/instruction.o \
		src/label.o \
		src/m.o

LIB=lib/libx64asm.a

BIN=bin/x64asm

##### TOP LEVEL TARGETS (release is default)

all: release

debug:
	make -C . erthing FLEXOPS="-d" BISONOPS="-t --report=state" OPT="-g"
release:
	make -C . erthing OPT="-DNDEBUG -O3"
profile:
	make -C . erthing OPT="-DNDEBUG -O3 -pg"

erthing: $(LIB) $(BIN)

##### BUILD TARGETS

codegen:
	cd src && \
		ghc Codegen.hs && \
		./Codegen && \
		rm -f *.hi *.o Codegen
	flex $(FLEXOPS) -Patt src/att.l 
	bison $(BISONOPS) -batt -patt --defines src/att.y && touch att.output 
	mv lex.*.* src/ && mv *.tab.* src/ && mv *.output src/
		
src/code.o: src/code.cc src/code.h codegen
	$(GCC) -w -fno-stack-protector -O0 $(INC) -c $< -o $@

src/%.o: src/%.cc src/%.h codegen
	$(GCC) $(OPT) $(INC) -c $< -o $@

##### LIBRARY TARGET

$(LIB): $(OBJ)
	ar rcs $@ $(OBJ)

##### BINARY TARGET

bin/%: tools/%.cc $(LIB)
	$(GCC) $(OPT) $< -o $@ $(INC) $(LIB)

##### TEST TARGET

check: 
	cd test/ && ./test.sh && cd -

##### CLEAN TARGETS

clean:
	rm -rf $(OBJ) $(LIB) $(BIN)
	rm -f doc/reference.html
	rm -f src/*.defn src/*.decl src/*.switch src/*.att src/*.enum src/*.table
	rm -f src/*.tab.c src/*.tab.h src/lex.*.c src/*.output
	rm -f test/*.s test/*.log test/*.o test/*.out
