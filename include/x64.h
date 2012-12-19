#ifndef X64_INCLUDE_X64_H
#define X64_INCLUDE_X64_H

#include "src/assembler/assembler.h"
#include "src/assembler/function.h"

#include "src/cfg/cfg.h"

#include "src/code/attributes.h"
#include "src/code/checker.h"
#include "src/code/code.h"
#include "src/code/instruction.h"
#include "src/code/op_accessor.h"
#include "src/code/op_set.h"
#include "src/code/op_type.h"
#include "src/code/opcode.h"

#include "src/io/att_reader.h"
#include "src/io/att_writer.h"
#include "src/io/intel_reader.h"
#include "src/io/intel_writer.h"

#include "src/operands/constants.h"
#include "src/operands/cr.h"
#include "src/operands/dr.h"
#include "src/operands/eflag.h"
#include "src/operands/imm.h"
#include "src/operands/label.h"
#include "src/operands/m.h"
#include "src/operands/mm.h"
#include "src/operands/modifier.h"
#include "src/operands/moffs.h"
#include "src/operands/operand.h"
#include "src/operands/r.h"
#include "src/operands/rel.h"
#include "src/operands/scale.h"
#include "src/operands/sreg.h"
#include "src/operands/st.h"
#include "src/operands/xmm.h"
#include "src/operands/ymm.h"

#include "src/stream/stream.h"

#endif
