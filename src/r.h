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

#ifndef X64ASM_SRC_R_H
#define X64ASM_SRC_R_H

#include <iostream>

#include "src/op_type.h"
#include "src/operand.h"

namespace x64asm {

class R64;

/** A general-purpose register. */
class R : public AtomicOperand {
	public:
		inline R(uint64_t val) : AtomicOperand{val} { }
		virtual ~R() = 0;
		virtual R64 parent() const;
		virtual bool check() const = 0;
		virtual void write_att(std::ostream& os) const = 0;
		virtual void write_intel(std::ostream& os) const = 0;
	private:
		virtual OpType type() const = 0;
};

/** One of the byte general-purpose registers: AL, CL, DL, BL. */
class Rl : public R {
	friend class Constants;
	public:
		virtual bool check() const;
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
	protected:
		inline Rl(uint64_t val) : R{val} { }
	private:
		virtual OpType type() const;
		virtual void insert_in(RegSet& os, bool promote = false) const;
};

/** One of the byte general-purpose registers: AH, CH, DH, BH. */
class Rh : public R {
	friend class Constants;
	public:
		virtual R64 parent() const;
		virtual bool check() const;
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
	private:
		inline Rh(uint64_t val) : R{val} { }
		virtual OpType type() const;
		virtual void insert_in(RegSet& os, bool promote = false) const;
};

/** One of the byte general-purpose registers: BPL, SPL, DIL and SIL; or one of 
	  the byte registers (R8B - R15B) available when using REX.R and 64-bit mode.
*/
class Rb : public R {
	friend class Constants;
	public:
		virtual bool check() const;
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
	private:
		inline Rb(uint64_t val) : R{val} { }
		virtual OpType type() const;
		virtual void insert_in(RegSet& os, bool promote = false) const;
};

/** The byte general-purpose register AL. */
class Al : public Rl {
	friend class Constants;
	public:
		virtual bool check() const;
	private:
		inline Al() : Rl{0} { }
		virtual OpType type() const;
};

/** The byte general-purpose register CL. */
class Cl : public Rl {
	friend class Constants;
	public:
		virtual bool check() const;
	private:
		inline Cl() : Rl{1} { }
		virtual OpType type() const;
};

/** One of the word general-purpose registers: AX, CX, DX, BX, SP, BP, SI, DI; 
	  or one of the word registers (R8W - R15W) available when using REX.R and 
		64-bit mode.
*/
class R16 : public R {
	friend class Constants;
	public:
		virtual bool check() const;
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
	protected:
		inline R16(uint64_t val) : R{val} { }
	private:
		virtual OpType type() const;
		virtual void insert_in(RegSet& os, bool promote = false) const;
};

/** The word general-purpose register AX. */
class Ax : public R16 {
	friend class Constants;
	public:
		virtual bool check() const;
	private:
		inline Ax() : R16{0} { }
		virtual OpType type() const;
};

/** The word general-purpose register DX. */
class Dx : public R16 {
	friend class Constants;
	public:
		virtual bool check() const;
	private:
		inline Dx() : R16{2} { }
		virtual OpType type() const;
};

/** One of the double or quadword general-purpose register which may
	  be used to form an address in memory.
*/
class AddrR : public R {
	public:
		AddrR(uint64_t val) : R{val} { }
		virtual ~AddrR() = 0;
		virtual bool check() const = 0;
		virtual void write_att(std::ostream& os) const = 0;
		virtual void write_intel(std::ostream& os) const = 0;
	private:
		virtual OpType type() const = 0;
};

/** One of the doubleword general-purpose registers: EAX, ECX, EDX, EBX, ESP, 
	  EBP, ESI, EDI; or one of the doubleword registers (R8D - R15D) available 
		when using REX.R in 64-bit mode.
*/
class R32 : public AddrR {
	friend class Constants;
	public:
		virtual bool check() const;
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
	protected:
		inline R32(uint64_t val) : AddrR{val} { }
	private:
		virtual OpType type() const;
		virtual void insert_in(RegSet& os, bool promote = false) const;
};

/** The doubleword general-purpose register EAX. */
class Eax : public R32 {
	friend class Constants;
	public:
		virtual bool check() const;
	private:
		inline Eax() : R32{0} { }
		virtual OpType type() const;
};

/** One of the quadword general-purpose registers: RAX, RBX, RCX, RDX, RDI, RSI,
	  RBP, RSP, R8–R15. These are available when using REX.R and 64-bit mode.
*/
class R64 : public AddrR {
	friend class Constants;
	friend class R;
	friend class Rh;
	public:
		virtual bool check() const;
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
	protected:
		inline R64(uint64_t val) : AddrR{val} { }
	private:
		virtual OpType type() const;
		virtual void insert_in(RegSet& os, bool promote = false) const;
};

/** The quadword general-purpose register RAX. */
class Rax : public R64 {
	friend class Constants;
	public:	
		virtual bool check() const;
	private:
		inline Rax() : R64{0} { }
		virtual OpType type() const;
};

} // namespace x64asm

#endif
