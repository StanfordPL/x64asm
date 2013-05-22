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

GCC=ccache g++ -std=c++0x 

OPT=-Werror -O3 -g

INC=-I. -I../../

OBJ=src/bit_gen.o src/cpu_state.o

DEP=../../lib/libx64asm.a

LIB=lib/libtrace.a

BIN=bin/trace 

DOC=doc/html

##### TOP LEVEL TARGETS (sse is default)

all: sse

sse:
	make -C . erthing SSE=CPU_STATE_SSE
avx:
	make -C . erthing SSE=CPU_STATE_AVX

erthing: $(LIB) $(BIN) $(DOC)

##### BUILD TARGETS

src/%.o: src/%.cc src/%.h 
	$(GCC) $(OPT) $(INC) -D$(SSE) -c $< -o $@

##### DOCUMENTATION TARGETS

doc/html: src/doxyfile src/* src/mainpage.dox
	doxygen src/doxyfile

src/mainpage.dox: README.txt	
	echo "/**" > src/mainpage.dox &&\
	echo \\mainpage >> src/mainpage.dox &&\
	echo "\\\verbatim" >> src/mainpage.dox &&\
	cat README.txt >> src/mainpage.dox &&\
	echo \\endverbatim >> src/mainpage.dox &&\
	echo "*/" >> src/mainpage.dox

##### LIBRARY_TARGET

$(LIB): $(OBJ)
	ar rcs $@ $(OBJ)

##### BINARY TARGET

bin/%: tools/%.cc $(LIB)
	$(GCC) $(OPT) $< -o $@ $(INC) -D$(SSE) $(LIB) $(DEP)

##### CLEAN TARGETS

clean:
	rm -rf $(DOC) $(OBJ) $(LIB) $(BIN)
	rm -f src/mainpage.dox
	rm -f src/*.o
