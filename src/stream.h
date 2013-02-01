/*
Copyright 2103 eric schkufza

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

#ifndef X64ASM_SRC_STREAM_H
#define X64ASM_SRC_STREAM_H

#include <iostream>
#include <stdint.h>

#include "src/code.h"
#include "src/env_bits.h"
#include "src/env_reg.h"
#include "src/instruction.h"
#include "src/op_set.h"
#include "src/operand.h"

namespace x64asm {

/** I/O syntax for iostreams. */
enum class Syntax : uint32_t {
	ATT = 0,
	INTEL
};

} // namespace x64asm

std::istream& operator>>(std::istream& is, const x64asm::Syntax s);
std::ostream& operator<<(std::ostream& os, const x64asm::Syntax s);

std::istream& operator>>(std::istream& is, x64asm::Code& c);

std::ostream& operator<<(std::ostream& os, const x64asm::Code& c);
std::ostream& operator<<(std::ostream& os, const x64asm::EnvBits& b);
std::ostream& operator<<(std::ostream& os, const x64asm::EnvReg& r);
std::ostream& operator<<(std::ostream& os, const x64asm::Instruction& i);
std::ostream& operator<<(std::ostream& os, const x64asm::Operand& o);
std::ostream& operator<<(std::ostream& os, const x64asm::OpSet& o);

#endif
