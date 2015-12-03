##### LATENCY TARGETS

OPTS=-O0 -g

latency: latency.cc
	g++ $(OPTS) -std=c++11 latency.cc -I.. ../lib/libx64asm.a -o latency

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
