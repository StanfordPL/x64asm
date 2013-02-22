all: constants context memcpy 

constants: constants.cc
	g++ -std=c++0x constants.cc -I.. ../lib/libx64asm.a -o constants

context: context.cc
	g++ -std=c++0x context.cc -I.. ../lib/libx64asm.a -o context

memcpy: memcpy.cc
	g++ -std=c++0x memcpy.cc -I.. ../lib/libx64asm.a -o memcpy

clean:
	rm -f constants context memcpy 
