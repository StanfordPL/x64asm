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

#ifndef X64ASM_ENV_REG_H
#define X64ASM_ENV_REG_H

#include <stdint.h>

namespace x64asm {

/** An environment register. */
class EnvReg {
	friend class RegSet;
	public:
		inline EnvReg(uint64_t val) : val_{val} { }
		virtual ~EnvReg() = 0;
	private:
		uint64_t val_;
};

/** The FPU Data register. */
class FpuData : public EnvReg {
	friend class Constants;
	private:
		inline FpuData() : EnvReg{0} { }
};

/** The FPU Instruction register. */
class FpuInstruction : public EnvReg {
	friend class Constants;
	private:
		inline FpuInstruction() : EnvReg{0} { }
};

/** The FPU Opcode regiter. */
class FpuOpcode : public EnvReg {
	friend class Constants;
	private:
		inline FpuOpcode() : EnvReg{0} { }
};

/** The instruction pointer register. */
class Rip : public EnvReg {
	friend class Constants;
	private:
		inline Rip() : EnvReg{0} { }
};

} // namespace x64asm

#endif

