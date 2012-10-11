##### CONSTANT DEFINITIONS

GCC=ccache g++

INC=-I./

OBJ=build/cfg/control_flow_graph.o \
		\
		build/assembler/assembler.o \
		\
		build/code/cond_reg.o \
		build/code/fp_reg.o \
		build/code/gp_reg.o \
		build/code/instruction.o \
		build/code/opcode.o \
		build/code/mmx_reg.o \
		build/code/reader.o \
		build/code/reg_set.o \
		build/code/scale.o \
		build/code/seg_reg.o \
		build/code/writer.o \
		build/code/xmm_reg.o \
		\
		build/sandboxer/sandboxer.o \
		\
		build/stream/stream.o \
		\
		build/tracer/tracer.o

BIN=bin/att_exec \
		bin/att_sandbox \
		bin/att_trace \
		bin/att2dot \
		bin/att2hex \
		bin/att2bin

DOC=doc/html

LIB=lib/libx64.a

##### TOP LEVEL TARGETS (release is default)

all: release

debug:
	make -C . erthing FLEXOPS="-d" BISONOPS="-t" OPT="-std=c++0x -Wall -fPIC -g"
release:
	make -C . erthing OPT="-std=c++0x -Wall -fPIC -DNDEBUG -O3 -funroll-all-loops"
profile:
	make -C . erthing OPT="-std=c++0x -Wall -fPIC -DNDEBUG -O3 -funroll-all-loops -pg"

erthing: $(BIN) $(DOC) $(LIB)

##### TEST TARGETS

test: test/objdump_file release

test/objdump_file: $(OBJ) $(BIN) test/enumerate_all.hs test/run.py
	$(MAKE) -C test
	@echo
	@echo "***********************************************"
	@echo
	@echo "If this compilation process succeeded, do"
	@echo "'make runtests' to continue."
	@echo
	@echo "***********************************************"
	@echo

runtests: test
	$(MAKE) test -C test

##### CLEAN TARGETS

clean:
	rm -rf $(BIN) $(DOC) $(LIB) build/* src/gen
	$(MAKE) clean -C test

##### EXTERNAL AND CODEGEN TARGETS

src/gen: src/codegen/*.hs src/codegen/*.csv src/codegen/att.l src/codegen/att.y
	mkdir -p build/codegen
	cd src/codegen && \
		ghc -W cfg.hs Instr.hs -o ../../build/codegen/cfg && \
		ghc assembler.hs Instr.hs -o ../../build/codegen/assembler && \
		rm *.hi *.o
	mkdir -p src/gen
	cd build/codegen && \
		./cfg ../../src/codegen/x64.csv && \
		./assembler ../../src/codegen/x64.csv && \
		mv assembler.* ../../src/gen/ && \
		mv opcode.* ../../src/gen/
	flex $(FLEXOPS) -Patt src/codegen/att.l && \
		mv lex.att.c src/gen
	bison $(BISONOPS) -batt -patt --defines src/codegen/att.y && \
		mv att.tab.h src/gen && \
		mv att.tab.c src/gen
		
##### DOCUMENTATION TARGETS

doc/html: doxyfile src/cfg/*.cc src/cfg/*.h src/assembler/*.cc src/assembler/*.h src/code/*.h src/code/*.cc
	doxygen doxyfile

##### BUILD TARGETS

build/assembler/%.o: src/assembler/%.cc src/assembler/%.h src/gen
	mkdir -p build/assembler && $(GCC) $(OPT) $(INC) -c $< -o $@
build/cfg/%.o: src/cfg/%.cc src/cfg/%.h src/gen
	mkdir -p build/cfg && $(GCC) $(OPT) $(INC) -c $< -o $@
build/code/%.o: src/code/%.cc src/code/%.h src/gen
	mkdir -p build/code && $(GCC) $(OPT) $(INC) -c $< -o $@
build/sandboxer/%.o: src/sandboxer/%.cc src/sandboxer/%.h src/gen
	mkdir -p build/sandboxer && $(GCC) $(OPT) $(INC) -c $< -o $@
build/stream/%.o: src/stream/%.cc src/stream/%.h src/gen
	mkdir -p build/stream && $(GCC) $(OPT) $(INC) -c $< -o $@
build/tracer/%.o: src/tracer/%.cc src/tracer/%.h src/gen
	mkdir -p build/tracer && $(GCC) $(OPT) $(INC) -c $< -o $@

##### LIBRARY TARGET

$(LIB): $(OBJ)
	ar rcs $@ $(OBJ)

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
