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

ifndef COMPILERBINARY
	COMPILERBINARY=g++
endif
GCC=ccache ${COMPILERBINARY} -Werror -Wextra -Wall -Wfatal-errors -pedantic -Wno-unused-parameter -Wno-reorder -std=c++11 -fPIC

INC=-I./
		
OBJ=src/assembler.o \
		src/code.o \
		src/env_bits.o \
		src/flag.o \
		src/flag_set.o \
		src/instruction.o \
		src/linker.o \
		src/m.o \
		src/opcode.o \
		src/operand.o \
		src/parser.o \
		src/r.o \
		src/reg_set.o \
		src/sreg.o \
		src/sse.o \
		src/type.o \
		src/mm.o \
		src/xmm.o \
		src/ymm.o

LIB=lib/libx64asm.a

BIN=bin/asm \
		bin/fuzz

##### TOP LEVEL TARGETS (release is default)

all: release

debug:
	$(MAKE) -C . erthing BISONOPS="-t --report=state" OPT="-g"
debug_flex:
	$(MAKE) -C . erthing FLEXOPS="-d" BISONOPS="-t --report=state" OPT="-g"
release:
	$(MAKE) -C . erthing OPT="-DNDEBUG -O3"
profile:
	$(MAKE) -C . erthing OPT="-DNDEBUG -O3 -pg"

erthing: $(LIB) $(BIN)

##### BUILD TARGETS

src/Codegen: src/Codegen.hs src/x86.csv
	cd src && \
		ghc Codegen.hs && \
		./Codegen && \
		rm -f *.hi *.o

src/parser.o: src/parser.cc src/instruction.cc src/instruction.h src/Codegen
	$(GCC) -w -O0 -fno-stack-protector $(INC) -c $< -o $@
src/%.o: src/%.cc src/%.h src/Codegen src/*.h
	$(GCC) $(OPT) $(INC) -c $< -o $@

##### LIBRARY TARGET

$(LIB): $(OBJ)
	ar rcs $@ $(OBJ)

##### BINARY TARGETS

bin/%: tools/%.cc $(LIB)
	$(GCC) $(OPT) $< -o $@ $(INC) $(LIB)

##### TEST TARGET

check: $(BIN)
	bin/fuzz 1000000

##### CLEAN TARGETS

clean:
	rm -rf $(OBJ) $(LIB) $(BIN) src/Codegen src/*.hi src/*.o
	rm -f src/*.defn src/*.decl src/*.switch src/*.att src/*.enum src/*.table
	rm -f src/*.tab.c src/*.tab.h src/lex.*.c src/*.output
	rm -f src/opcode.intel src/opcode.l32_transform src/opcode.names
	rm -f test/*.s test/*.log test/*.o test/*.out
