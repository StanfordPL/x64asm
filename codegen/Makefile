##### LATENCY TARGETS

OPTS=-O0 -g

all: haswell_latency.inc

latency: latency.cc cpu_info.cc cpu_info.h
	g++ $(OPTS) -std=c++11 latency.cc cpu_info.cc -I.. ../lib/libx64asm.a -o latency

haswell_latency.inc: latency
	@echo "Are you the only one on the system?"
	@echo "Have you put the processor into performance mode (to disable scaling)?"
	@echo "Killed unnecessary processes?"
	@echo "... if you haven't, ctrl-C and fix up."
	@echo ""
	rm -f haswell_latency.inc
	./latency | tee haswell_latency.inc

##### CLEAN TARGETS

clean_src:
	rm -f haswell_latency.inc

clean:
	rm -f latency
