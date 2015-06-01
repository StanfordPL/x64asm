#include <iostream>
#include <string>

#include "include/x64asm.h"

using namespace std;
using namespace x64asm;
using namespace cpputil;

/** A simple test program. Reads att syntax and prints human readable hex. */
int main(int argc, char** argv) {
  Code c;
  cin >> c;

  if(failed(cin)) {
    cerr << endl << "Parse error encountered." << endl;
    cerr << fail_msg(cin);
    return 1;
  }

  cout << endl;
  cout << "Assembling..." << endl << c << endl << endl << endl;
  
  cout << Assembler().assemble(c) << endl;

  return 0;
}
