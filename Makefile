##### CONSTANT DEFINITIONS

GCC=ccache g++

INC=-I./
		
OBJ=build/assembler.o \
		build/cfg.o \
		build/code.o \
		build/constants.o \
		build/cr.o \
		build/dr.o \
		build/eflag.o \
		build/imm.o \
		build/instruction.o \
		build/label.o \
		build/m.o \
		build/mm.o \
		build/modifier.o \
		build/moffs.o \
		build/operand.o \
		build/op_set.o \
		build/r.o \
		build/rel.o \
		build/sreg.o \
		build/stream.o \
		build/st.o \
		build/xmm.o \
		build/ymm.o

LIB=lib/libx64.a

TEST=test/constants

BIN=#bin/att_exec \
		bin/att_sandbox \
		bin/att_trace \
		bin/att2dot \
		bin/att2hex \
		bin/att2bin

DOC=doc/html

##### TOP LEVEL TARGETS (release is default)

all: release

debug:
	make -C . erthing FLEXOPS="-d" BISONOPS="-t" OPT="-std=c++0x -Wall -fPIC -g"
release:
	make -C . erthing OPT="-std=c++0x -Wall -fPIC -DNDEBUG -O3 -funroll-all-loops"
profile:
	make -C . erthing OPT="-std=c++0x -Wall -fPIC -DNDEBUG -O3 -funroll-all-loops -pg"

erthing: $(LIB) $(TEST) $(BIN) $(DOC)

##### TEST TARGETS

test: erthing
	cd test/stokeasm_py; python setup.py build; rm *.so; find -name "*.so" -print0 | xargs -0 /bin/cp -f -t .
	cd test; ./run_tests.sh

##### CLEAN TARGETS

clean:
	rm -rf build/* $(LIB) $(TEST) $(BIN) $(DOC)
	rm -f src/assembler.defn src/assembler.decl src/assembler.switch
	rm -f src/opcode.att src/opcode.enum src/*.table
	rm -rf test/enumerate_all.hi test/enumerate_all.o test/tmp/* test/enumerate_all
	rm -f test/stokeasm_py/*.so
	rm -rf test/stokeasm_py/build

##### BUILD TARGETS

codegen: src/assembler.defn src/Codegen.hs src/x86.csv

src/assembler.defn: src/Codegen.hs src/x86.csv 
	cd src && \
		ghc Codegen.hs && \
		./Codegen x86.csv && \
		rm -f *.hi *.o Codegen

#	flex $(FLEXOPS) -Patt src/codegen/att.l && \
		mv lex.att.c src/gen
#	bison $(BISONOPS) -batt -patt --defines src/codegen/att.y && \
		mv att.tab.h src/gen && \
		mv att.tab.c src/gen
		
build/%.o: src/%.cc src/%.h codegen
	$(GCC) $(OPT) $(INC) -c $< -o $@

##### DOCUMENTATION TARGETS

doc/html: doxyfile src/* 
	doxygen doxyfile

##### LIBRARY TARGET

$(LIB): $(OBJ)
	ar rcs $@ $(OBJ)

##### TEST TARGET

test/constants: test/constants.cc $(LIB)
	$(GCC) $(OPT) $< -o $@ $(INC) $(LIB)

##### BINARY TARGET

bin/att_exec: tools/att_exec.cc $(LIB)
	$(GCC) $(OPT) $< -o $@ $(INC) $(LIB)
bin/att_sandbox: tools/att_sandbox.cc $(LIB)
	$(GCC) $(OPT) $< -o $@ $(INC) $(LIB)
bin/att_trace: tools/att_trace.cc $(LIB)
	$(GCC) $(OPT) $< -o $@ $(INC) $(LIB)
bin/att2dot: tools/att2dot.cc $(LIB)
	$(GCC) $(OPT) $< -o $@ $(INC) $(LIB)
bin/att2hex: tools/att2hex.cc $(LIB)
	$(GCC) $(OPT) $< -o $@ $(INC) $(LIB)
bin/att2bin: tools/att2bin.cc $(LIB)
	$(GCC) $(OPT) $< -o $@ $(INC) $(LIB)
