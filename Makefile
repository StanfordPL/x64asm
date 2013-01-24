##### CONSTANT DEFINITIONS

GCC=ccache g++

INC=-I./
		
OBJ=build/code/assembler.o \
		build/code/cfg.o \
		build/code/code.o \
		build/code/constants.o \
		build/code/cr.o \
		build/code/dr.o \
		build/code/eflag.o \
		build/code/imm.o \
		build/code/instruction.o \
		build/code/label.o \
		build/code/m.o \
		build/code/mm.o \
		build/code/modifier.o \
		build/code/moffs.o \
		build/code/operand.o \
		build/code/op_set.o \
		build/code/r.o \
		build/code/rel.o \
		build/code/sreg.o \
		build/code/stream.o \
		build/code/st.o \
		build/code/xmm.o \
		build/code/ymm.o

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
	rm -f src/code/assembler.defn src/code/assembler.decl src/code/assembler.switch
	rm -f src/code/opcode.att src/code/opcode.enum src/code/*.table
	rm -rf test/enumerate_all.hi test/enumerate_all.o test/tmp/* test/enumerate_all
	rm -f test/stokeasm_py/*.so
	rm -rf test/stokeasm_py/build

##### BUILD TARGETS

codegen: src/code/assembler.defn src/code/Codegen.hs src/code/x86.csv

src/code/assembler.defn: src/code/Codegen.hs src/code/x86.csv 
	cd src/code && \
		ghc Codegen.hs && \
		./Codegen x86.csv && \
		rm -f *.hi *.o Codegen

#	flex $(FLEXOPS) -Patt src/codegen/att.l && \
		mv lex.att.c src/gen
#	bison $(BISONOPS) -batt -patt --defines src/codegen/att.y && \
		mv att.tab.h src/gen && \
		mv att.tab.c src/gen
		
build/code/%.o: src/code/%.cc src/code/%.h codegen
	mkdir -p build/code && $(GCC) $(OPT) $(INC) -c $< -o $@

##### DOCUMENTATION TARGETS

doc/html: doxyfile src/code/* 
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
