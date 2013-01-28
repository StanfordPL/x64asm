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

#ifndef X64ASM_SRC_ST_H
#define X64ASM_SRC_ST_H

#include <iostream>

#include "src/op_type.h"
#include "src/operand.h"

namespace x64asm {

/** The ith element from the top of the FPU register stack 
	  (i = 0 through 7). 
*/
class St : public AtomicOperand {
	friend class Constants;
	public:
		virtual bool check() const;
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
	protected:
		inline St(uint64_t val) : AtomicOperand{val} { } 
	private:
		virtual OpType type() const;
};

/** The top element of the FPU register stack. */
class St0 : public St {
	friend class Constants;
	public:
		virtual bool check() const;
	private:
		inline St0() : St{0} { }
		virtual OpType type() const;
};

} // namespace x64asm

#endif
