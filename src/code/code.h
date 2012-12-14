#ifndef X64_SRC_CODE_CODE_H
#define X64_SRC_CODE_CODE_H

#include <vector>

#include "src/code/instruction.h"

namespace x64 {

/** A sequence of Instructions. */
typedef std::vector<Instruction> Code;

} // namespace x64

#endif
