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
	rm -f $src.log

	while read line           
	do           
    echo $line > test.s
		g++ -c test.s

		objdump -d -Msuffix test.o | tail -n+8 | cut -c 7-27 | sed 's/ *$//' > objdump.out
		cat test.s | ../bin/x64asm -i att -o hex | sed 's/ *$//' > x64asm.out

		diff objdump.out x64asm.out > /dev/null
		if [ $? -ne 0 ]
		then
			cat test.s >> $src.log
			cat objdump.out >> $src.log
			cat x64asm.out >> $src.log
			echo "****************************************" >> $src.log
		fi

	done < $src

	echo "  Results: "`grep "*" $src.log | wc -l`" errors"
done

echo "Done Running Tests"
echo ""
echo "  Results: "`grep "*" error.log | wc -l`" error (see .log files for details)"
echo "  Expected: [LOTS]"

rm -f test.s test.o objdump.out x64asm.out

exit 0
