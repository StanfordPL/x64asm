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

#ifndef X64ASM_ENV_BITS_H
#define X64ASM_ENV_BITS_H

#include <iostream>

namespace x64asm {

/** An environment register bit. */
class EnvBits {
	public:
		inline EnvBits(size_t index, size_t width) 
				: index_{index}, width_{width} {
		}
		virtual ~EnvBits() = 0;
		inline size_t index() const { return index_; }
		inline size_t width() const { return width_; }
		virtual void write_att(std::ostream& os) const = 0;
		virtual void write_intel(std::ostream& os) const = 0;
	private:
		size_t index_;
		size_t width_;
};

/** An EFLAGS register bit. */
class Eflags : public EnvBits {
	friend class Constants;
	public:
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
	private:
		inline Eflags(size_t index, size_t width) : EnvBits{index, width} { }
};

/** An FPU control register bit. */
class FpuControl : public EnvBits {
	friend class Constants;
	public:
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
	private:
		inline FpuControl(size_t index, size_t width) : EnvBits{index, width} { }
};

/** An FPU status register bit. */
class FpuStatus : public EnvBits {
	friend class Constants;
	public:
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
	private:
		inline FpuStatus(size_t index, size_t width) : EnvBits{index, width} { }
};

/** An FPU tag register. */
class FpuTag : public EnvBits {
	friend class Constants;
	public:
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
	private:
		inline FpuTag(size_t index, size_t width) : EnvBits{index, width} { }
};

/** An MXCSR register bit. */
class Mxcsr : public EnvBits {
	friend class Constants;
	public:
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
	private:
		inline Mxcsr(size_t index, size_t width) : EnvBits{index, width} { }
};

} // namespace x64asm

#endif

