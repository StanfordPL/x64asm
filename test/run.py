#!/usr/bin/python
import subprocess
import multiprocessing
import sys

sys.path.append("stokeasm_py")

from stokeasm import att_exec
from stokeasm import att_hex


def red(text):
    output_file.write("\033[31m" + str(text) + "\033[0m\n")

def yellow(text):
    output_file.write("\033[35m" + str(text) + "\033[0m\n")

def green(text):
    output_file.write("\033[32m" + str(text) + "\033[0m\n")

def write(text):
    output_file.write(text + "\n")
### RUN A TEST ###


passes = 0
warns = 0
fails = 0

def test_pass(instruction):
    global passes
    passes = passes + 1

def test_fail(instruction, expected, got, errors):
    global fails
    fails = fails + 1
    red("FAIL: " + instruction.strip())
    write( "Expected: " + expected )
    write( "Got:      " + got )
    write( "Errors: " + errors )

def test_warn(instruction, expected, got, errors, message):
    global warns
    yellow("WARN: " + instruction.strip())
    write( "Reason: " + message )
    write( "Expected: " + expected )
    write( "Got: " + got )
    warns = warns + 1


def run_test(hexstring, instruction):
    #call = subprocess.Popen("../bin/att2hex",stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    #call.stdin.write(instruction)
    #results = call.communicate()
    #call.stdin.close()
    #got = results[0].strip()
    got = att_hex(instruction).strip()
    
		#output_file.write(instruction)
		#output_file.write(got)
    errors = ""

    if ( hexstring == got ):
        test_pass(instruction)
    elif ( hexstring == got[3:] ):
        test_warn(instruction, hexstring, got, errors, "Missing first byte")
    elif ( hexstring[3:] == got ):
        test_warn(instruction, hexstring, got, errors, "Extra first byte")
    elif ( len(hexstring) >= 6 and len(got) >= 6 and hexstring[0:3] == got[3:6] and hexstring[3:6] == got[0:3]):
        test_warn(instruction, hexstring, got, errors, "First two bytes swapped.")
    elif ( len(hexstring) >= 9 and len(got) >= 9 and hexstring[3:6] == got[6:9] and hexstring[6:9] == got[3:6]):
        test_warn(instruction, hexstring, got, errors, "Second and third bytes swapped.")
    else:
        test_fail(instruction, hexstring, got, errors)

### OPEN THE OBJDUMP OUTPUT ###

hexstring = ""
instruction = ""

output_file = None
objdump_file = None
try:
  instruction_file = open(sys.argv[1] + ".s", 'r')
  objdump_file = open(sys.argv[1] + ".dump", 'r')
  output_file = open(sys.argv[1] + ".output", 'w')
except:
  print "Need to specify prefix as command line argument."

for line in objdump_file:
    tmp_instruction = line[22:].strip()
    if tmp_instruction == "":
        hexstring += line[:22].strip() + " "
    else:
        #run the test
        if instruction != "":
            real_instruction = instruction_file.readline()
            run_test(hexstring.strip(), real_instruction + "\n")

        #prepare for next test
        instruction = tmp_instruction
        hexstring = line[:22].strip() + " "
        
total = passes + fails + warns
green("Passes: " + str(passes) + " (" + str(passes*100/total) + "%)")
yellow("Warns: " + str(warns) + " (" + str(warns*100/total) + "%)")
red("Fails: " + str(fails) + " (" + str(fails*100/total) + "%)")
write("Total tests: " + str(total))
output_file.flush()
output_file.close()
instruction_file.close()
objdump_file.close()
