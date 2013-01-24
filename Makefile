##### CONSTANT DEFINITIONS

GCC=ccache g++

INC=-I./
		
OBJ=src/assembler.o \
		src/cfg.o \
		src/code.o \
		src/constants.o \
		src/cr.o \
		src/dr.o \
		src/eflag.o \
		src/imm.o \
		src/instruction.o \
		src/label.o \
		src/m.o \
		src/mm.o \
		src/modifier.o \
		src/moffs.o \
		src/operand.o \
		src/op_set.o \
		src/r.o \
		src/rel.o \
		src/sreg.o \
		src/stream.o \
		src/st.o \
		src/xmm.o \
		src/ymm.o

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
		
src/%.o: src/%.cc src/%.h codegen
	$(GCC) $(OPT) $(INC) -c $< -o $@

##### DOCUMENTATION TARGETS

doc/html: doc/doxyfile src/* 
	doxygen doc/doxyfile

##### LIBRARY TARGET

$(LIB): $(OBJ)
	ar rcs $@ $(OBJ)

##### TEST TARGET

test/constants: test/constants.cc $(LIB)
	$(GCC) $(OPT) $< -o $@ $(INC) $(LIB)

##### BINARY TARGET

bin/%: tools/%.cc $(LIB)
	$(GCC) $(OPT) $< -o $@ $(INC) $(LIB)

##### CLEAN TARGETS

clean:
	rm -rf $(DOC) $(OBJ) $(LIB) $(BIN) $(TEST)
	rm -f src/*.defn src/*.decl src/*.switch src/*.att src/*.enum src/*.table
	rm -rf test/enumerate_all.hi test/enumerate_all.o test/tmp/* test/enumerate_all
	rm -f test/stokeasm_py/*.so
	rm -rf test/stokeasm_py/build


