all: memcpy

memcpy: memcpy.cc
	g++ -std=c++0x memcpy.cc -I.. ../lib/libx64asm.a -o memcpy

clean:
	rm -f memcpy
