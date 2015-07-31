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
  
  auto result = Assembler().assemble(c);
  if(!result.first) {
    cout << "Could not assemble; 8-bit jump offset was given but target was further away." << endl;
  } else {
    cout << result.second << endl;
  }

  return 0;
}
