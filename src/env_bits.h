/*
Copyright 2013 eric schkufza

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
#include <stdint.h>

namespace x64asm {

/** An environment register bit. */
class EnvBits {
  public:
		/** Copy constructor. */
		EnvBits(const EnvBits& rhs) : index_(rhs.index_), width_(rhs.width_) {
		}
		/** Move constructor. */
		EnvBits(const EnvBits&& rhs) : index_(rhs.index_), width_(rhs.width_) {
		}
		/** Assignment operator. */
		EnvBits& operator=(const EnvBits& rhs) {
			EnvBits(rhs).swap(*this);
			return *this;
		}
		/** Move assignment operator. */
		EnvBits& operator=(const EnvBits&& rhs) {
			EnvBits(std::move(rhs)).swap(*this);
			return *this;
		}

    /** Returns this bit's upper register index. */
    constexpr size_t index() {
			return index_;
		}
    /** Returns the number of bits this register bit spans. */
    constexpr size_t width() {
			return width_;
		}

		/** STL-compliant swap. */
		void swap(EnvBits& rhs) {
			std::swap(index_, rhs.index_);
			std::swap(width_, rhs.width_);
		}

  protected:
    /** Direct access to this constructor is disallowed. */
    constexpr EnvBits(size_t i, size_t w) : index_(i), width_(w) {
		}

  private:
    /** This bit's upper register index. */
    uint32_t index_;
    /** The number of bits this register bit spans. */
    uint32_t width_;
};

/** An EFLAGS register bit. */
class Eflags : public EnvBits {
    // Needs access to constructor.
    friend class Constants;

  private:
    /** Direct access to this constructor is disallowed. */
    constexpr Eflags(size_t i, size_t w) : EnvBits(i, w) {
		}
};

/** An FPU control register bit. */
class FpuControl : public EnvBits {
    // Needs access to constructor.
    friend class Constants;

  private:
    /** Direct access to this constructor is disallowed. */
    constexpr FpuControl(size_t i, size_t w) : EnvBits(i, w) {
		}
};

/** An FPU status register bit. */
class FpuStatus : public EnvBits {
    // Needs access to constructor.
    friend class Constants;

  private:
    /** Direct access to this constructor is disallowed. */
    constexpr FpuStatus(size_t i, size_t w) : EnvBits(i, w) {
		}
};

/** An FPU tag register. */
class FpuTag : public EnvBits {
    // Needs access to constructor.
    friend class Constants;

  private:
    /** Direct access to this constructor is disallowed. */
    constexpr FpuTag(size_t i, size_t w) : EnvBits(i, w) {
		}
};

/** An MXCSR register bit. */
class Mxcsr : public EnvBits {
    // Needs access to constructor.
    friend class Constants;

  private:
    /** Direct access to this constructor is disallowed. */
    constexpr Mxcsr(size_t i, size_t w) : EnvBits(i, w) {
		}
};

} // namespace std

namespace std {

// STL-compliant swap
inline void swap(x64asm::EnvBits& lhs, x64asm::EnvBits& rhs) {
	lhs.swap(rhs);
}

} // namespace std

#endif

