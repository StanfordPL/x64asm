EX = constants context dataflow functions hello

all: $(EX)

constants: constants.cc
	g++ -O3 -std=c++0x constants.cc -I.. ../lib/libx64asm.a -o constants

context: context.cc
	g++ -O3 -std=c++0x context.cc -I.. ../lib/libx64asm.a -o context

dataflow: dataflow.cc
	g++ -O3 -std=c++0x dataflow.cc -I.. ../lib/libx64asm.a -o dataflow

functions: functions.cc
	g++ -O3 -std=c++0x functions.cc -I.. ../lib/libx64asm.a -o functions

hello: hello.cc
	g++ -O3 -std=c++0x hello.cc -I.. ../lib/libx64asm.a -o hello

clean:
	rm -f $(EX)
