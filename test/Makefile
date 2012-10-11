
all: enumerate_all objdump_file

enumerate_all: enumerate_all.hs stokeasm_py/stokeasm.so
	ghc -i../src/codegen enumerate_all.hs

test.s: enumerate_all ../src/codegen/x64.csv
	./enumerate_all ../src/codegen/x64.csv > test.s

stokeasm_py/stokeasm.so: stokeasm_py/stokeasm.cc
	$(MAKE) -C stokeasm_py

objdump_file: test.s
	gcc -c test.s
	objdump -d -Msuffix test.o | cut -c11- | tail -n +8 > objdump_file
	- mkdir -p tmp
	- rm -f tmp/dump*
	split -d -l100000 objdump_file "tmp/dump"
	

test: objdump_file
	- rm -f tmp/*.output
	- find -H tmp -name "dump*" -print0 | xargs -0 -n1 -P16 -I Z ./run.py Z > /dev/null 2> /dev/null
	cat tmp/dump*.output > output
	@echo "PASS:"
	@grep Pass output | cut -c 14- | grep -o "^[0-9]*" | awk '{ sum += $$1 } END { print sum}' | cat -
	@echo "WARN:"
	@grep Warn output | cut -c 13- | grep -o "^[0-9]*" | awk '{ sum += $$1 } END { print sum}' | cat -
	@echo "FAIL:"
	@grep Fail output | cut -c 13- | grep -o "^[0-9]*" | awk '{ sum += $$1 } END { print sum}' | cat -

clean:
	- rm -f enumerate_all.hi
	- rm -f enumerate_all
	- rm -f enumerate_all.o
	- rm -f test.s
	- rm -f test.o
	- rm -f objdump_file
	- rm -f tmp/*
	$(MAKE) clean -C stokeasm_py
