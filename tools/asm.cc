#include <iostream>
#include <string>

#include "include/x64asm.h"

using namespace std;
using namespace x64asm;

/** Prints a parse error. */
int parse_error() {
  cerr << "Unable to read input file!" << endl;
  return 1;
}

/** A simple test program. Reads att syntax and prints human readable hex. */
int main(int argc, char** argv) {
  Code c;
  cin >> c;

  if (!cin.good())
    return parse_error();

  cout << endl;
  cout << "Assembling..." << endl << c << endl << endl << endl;
  
  cout << Assembler().assemble(c) << endl;

  return 0;
}
