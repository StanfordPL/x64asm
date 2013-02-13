#!/bin/sh

rm -f error.log

if [ ! -e "adc_al_imm8.s" ]
then
	echo "Unable to run tests, try running make to generate inputs!"
	exit 0
fi

for src in `ls *.s`
do
	echo "Running tests for "$src

	while read line           
	do           
    echo $line > test.s
		g++ -c test.s

		objdump -d -Msuffix test.o | tail -n+8 | cut -c 7-27 > objdump.out
		cat test.s | ../bin/x64asm -i att -o hex > x64asm.out

		diff objdump.out x64asm.out > /dev/null
		if [ $? -ne 0 ]
		then
			cat test.s >> error.log
			cat objdump.out >> error.log
			cat x64asm.out >> error.log
			echo "****************************************" >> error.log
		fi

	done < $src
done

echo "Results: "`grep "*" error.log | wc -l`" error (see error.log for details)"
echo "Expected: [LOTS]"

rm -f test.s test.o objdump.out x64asm.out

exit 0
