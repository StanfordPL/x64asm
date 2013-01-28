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

#ifndef X64ASM_SRC_CR_H
#define X64ASM_SRC_CR_H

#include <iostream>

#include "src/op_type.h"
#include "src/operand.h"

namespace x64asm {

/** A control register. */
class Cr : public AtomicOperand {
	public:
		virtual bool check() const;
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
	protected:
		inline Cr(uint64_t val) : AtomicOperand{val} { }
	private:
		virtual OpType type() const;
};

/** One of the control reigsters: CR0, CR2, CR3, CR4. */
class Cr0234 : public Cr {
	friend class Constants;
	public:
		virtual bool check() const;
	private:
		inline Cr0234(uint64_t val) : Cr{val} { }
		virtual OpType type() const;
};

/** The control register CR8 */
class Cr8 : public Cr {
	friend class Constants;
	public:
		virtual bool check() const;
	private:
		inline Cr8() : Cr{8} { }
		virtual OpType type() const;
};

} // namespace x64asm

#endif
