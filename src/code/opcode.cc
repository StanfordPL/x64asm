#include "src/code/opcode.h"

using namespace std;

namespace x64 { 

// Static array declarations
#include "src/gen/opcode.implicit"

// Domain declaration
#include "src/gen/opcode.domain"

const Opcode opcode_null = -1;

} // namespace x64
