CC  = g++ -std=c++0x
OPT = -Werror -O3
EX  = abi constants context dataflow functions hello linker

all: $(EX)

abi: abi.cc
	$(CC) $(OPT) abi.cc -I../ ../lib/libx64asm.a -o abi

constants: constants.cc
	$(CC) $(OPT) constants.cc -I../ ../lib/libx64asm.a -o constants

context: context.cc
	$(CC) $(OPT) context.cc -I../ ../lib/libx64asm.a -o context

dataflow: dataflow.cc
	$(CC) $(OPT) dataflow.cc -I../ ../lib/libx64asm.a -o dataflow

functions: functions.cc
	$(CC) $(OPT) functions.cc -I../ ../lib/libx64asm.a -o functions

hello: hello.cc
	$(CC) $(OPT) hello.cc -I../ ../lib/libx64asm.a -o hello

linker: linker.cc
	$(CC) $(OPT) linker.cc -I../ ../lib/libx64asm.a -o linker

clean:
	rm -f $(EX)
