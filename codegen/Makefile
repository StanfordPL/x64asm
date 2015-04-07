##### LATENCY TARGETS

latency: latency.cc
	g++ -std=c++11 latency.cc -I.. ../lib/libx64asm.a -o latency

haswell_latency: latency
	./latency > haswell_latency.inc

##### CLEAN TARGETS

clean_src:
	rm -f haswell_latency.inc

clean:
	rm -f latency
