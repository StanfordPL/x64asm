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

#include "src/operand.h"

namespace x64asm {

class R64;

/** A general-purpose register. */
class R : public Operand {
	public:
		R64 parent() const;

	protected:	
		constexpr R(uint64_t val) 
				: Operand{val} { 
		}
};

/** One of the byte general-purpose registers: AL, CL, DL, BL. */
class Rl : public R {
	friend class Constants;

	public:
		constexpr bool check() {
			return val_ < 4;
		}

		void write_att(std::ostream& os) const;
		void write_intel(std::ostream& os) const;

	protected:
		constexpr Rl(uint64_t val) 
				: R{val} { 
		}
};

/** One of the byte general-purpose registers: AH, CH, DH, BH. */
class Rh : public R {
	friend class Constants;

	public:
		R64 parent() const;

		constexpr bool check() {
			return val_ >= 4 && val_ < 8;
		}

		void write_att(std::ostream& os) const;
		void write_intel(std::ostream& os) const;

	private:
		constexpr Rh(uint64_t val) 
				: R{val} { 
		}
};

/** One of the byte general-purpose registers: BPL, SPL, DIL and SIL; or one of 
	  the byte registers (R8B - R15B) available when using REX.R and 64-bit mode.
*/
class Rb : public R {
	friend class Constants;

	public:
		constexpr bool check() {
			return val_ >= 4 && val_ < 16;
		}

		void write_att(std::ostream& os) const;
		void write_intel(std::ostream& os) const;

	private:
		constexpr Rb(uint64_t val) 
				: R{val} { 
		}

		void insert_in(RegSet& os, bool promote = false) const;
};

/** The byte general-purpose register AL. */
class Al : public Rl {
	friend class Constants;

	public:
		constexpr bool check() {
			return val_ == 0;
		}

	private:
		constexpr Al() 
				: Rl{0} { 
		}
};

/** The byte general-purpose register CL. */
class Cl : public Rl {
	friend class Constants;

	public:
		constexpr bool check() {
			return val_ == 1;
		}

	private:
		constexpr Cl() 
				: Rl{1} { 
		}
};

/** One of the word general-purpose registers: AX, CX, DX, BX, SP, BP, SI, DI; 
	  or one of the word registers (R8W - R15W) available when using REX.R and 
		64-bit mode.
*/
class R16 : public R {
	friend class Constants;

	public:
		constexpr bool check() {
			return val_ < 16;
		}

		void write_att(std::ostream& os) const;
		void write_intel(std::ostream& os) const;

	protected:
		constexpr R16(uint64_t val) 
				: R{val} { 
		}
};

/** The word general-purpose register AX. */
class Ax : public R16 {
	friend class Constants;

	public:
		constexpr bool check() {
			return val_ == 0;
		}

	private:
		constexpr Ax() 
				: R16{0} { 
		}
};

/** The word general-purpose register DX. */
class Dx : public R16 {
	friend class Constants;

	public:
		constexpr bool check() {
			return val_ == 2;
		}

	private:
		constexpr Dx() 
				: R16{2} { 
		}
};

/** One of the doubleword general-purpose registers: EAX, ECX, EDX, EBX, ESP, 
	  EBP, ESI, EDI; or one of the doubleword registers (R8D - R15D) available 
		when using REX.R in 64-bit mode.
*/
class R32 : public R {
	friend class Constants;
	friend class M;

	public:
		constexpr bool check() {
			return val_ < 16;
		}

		void write_att(std::ostream& os) const;
		void write_intel(std::ostream& os) const;

	protected:
		constexpr R32(uint64_t val) 
				: R{val} { 
		}
};

/** The doubleword general-purpose register EAX. */
class Eax : public R32 {
	friend class Constants;

	public:
		constexpr bool check() {
			return val_ == 0;
		}

	private:
		constexpr Eax() 
				: R32{0} { 
		}
};

/** One of the quadword general-purpose registers: RAX, RBX, RCX, RDX, RDI, RSI,
	  RBP, RSP, R8–R15. These are available when using REX.R and 64-bit mode.
*/
class R64 : public R {
	friend class Constants;
	friend class R;
	friend class Rh;

	public:
		constexpr bool check() {
			return val_ < 16;
		}

		void write_att(std::ostream& os) const;
		void write_intel(std::ostream& os) const;

	protected:
		constexpr R64(uint64_t val) 
				: R{val} { 
		}
};

/** The quadword general-purpose register RAX. */
class Rax : public R64 {
	friend class Constants;

	public:	
		constexpr bool check() {
			return val_ == 0;
		}

	private:
		constexpr Rax() 
				: R64{0} { 
		}
};

} // namespace x64asm

#endif
