all: constants memcpy 

constants: constants.cc
	g++ -std=c++0x constants.cc -I.. ../lib/libx64asm.a -o constants

memcpy: memcpy.cc
	g++ -std=c++0x memcpy.cc -I.. ../lib/libx64asm.a -o memcpy

clean:
	rm -f constants memcpy
