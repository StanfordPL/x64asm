CC  = g++ -std=c++0x
OPT = -Werror -O3

all: trace

clean:
	rm -f *.o trace

trace: trace.o state.o
	$(CC) $(OPT) -o trace trace.o state.o ../../lib/libx64asm.a

trace.o: trace.cc
	$(CC) $(OPT) -I../../ -c trace.cc -o trace.o

state.o: state.h state.cc
	$(CC) $(OPT) -I../../ -c state.cc -o state.o
