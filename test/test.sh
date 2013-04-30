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

		objdump -d test.o | tail -n+8 | cut -c 7-27 | sed 'N;s/\n//' | sed 's/ *$//' > objdump.out
		cat test.s | ../bin/x64asm -i att -o hex | sed 'N;s/\n//' | sed 's/ *$//' > x64asm.out

		diff objdump.out x64asm.out > /dev/null
		if [ $? -ne 0 -a -s objdump.out ]
		then
			cat test.s >> $src.log
			echo "reference: " >> $src.log
			cat objdump.out >> $src.log
			echo "x64asm: " >> $src.log
			cat x64asm.out >> $src.log
			echo "****************************************" >> $src.log
		fi

	done < $src

	if [ -e $src.log ]
	then
		echo "  Found "`grep "*" $src.log | wc -l`" errors"
	fi
done

echo "Done Running Tests"
echo ""
echo "  Results: "`cat *.log | grep "*" | wc -l`" error (see .log files for details)"
echo "  Expected: [LOTS]"

rm -f test.s test.o objdump.out x64asm.out

exit 0
