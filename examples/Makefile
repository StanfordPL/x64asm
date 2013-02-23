all: constants context functions hello

constants: constants.cc
	g++ -std=c++0x constants.cc -I.. ../lib/libx64asm.a -o constants

context: context.cc
	g++ -std=c++0x context.cc -I.. ../lib/libx64asm.a -o context

functions: functions.cc
	g++ -std=c++0x functions.cc -I.. ../lib/libx64asm.a -o functions

hello: hello.cc
	g++ -std=c++0x hello.cc -I.. ../lib/libx64asm.a -o hello

clean:
	rm -f constants context functions hello
