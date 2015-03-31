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

#ifndef X64ASM_ENV_REG_H
#define X64ASM_ENV_REG_H

#include <iostream>

namespace x64asm {

/** An environment register. */
class EnvReg {
public:
  /** @todo This method is undefined. */
  std::istream& read_text(std::istream& is) {
    is.setstate(std::ios::failbit);
    return is;
  }

  /** Comparison */
  constexpr bool operator==(const EnvReg& rhs) {
    return val_ == rhs.val_;
  }
  /** Comparison */
  constexpr bool operator!=(const EnvReg& rhs) {
    return !(*this == rhs);
  }
  /** Comparison */
  constexpr bool operator<(const EnvReg& rhs) {
    return val_ < rhs.val_;
  }

  /** STL-compliant hash */
  constexpr size_t hash() {
    return val_;
  }
  /** STL_compliant swap */
  void swap(EnvReg& rhs) {
    std::swap(val_, rhs.val_);
  }

protected:
  /** Direct access to this constructor is disallowed. */
  constexpr EnvReg(size_t val) : val_(val) {}

private:
  /** Globally unique id. */
  size_t val_;
};

/** The FPU Data register. */
class FpuData : public EnvReg {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Writes to an ostream using text. */
  std::ostream& write_text(std::ostream& os) const {
    return (os << "%data");
  }

private:
  /** Direct access to this constructor is disallowed. */
  constexpr FpuData() : EnvReg(0) {}
};

/** The FPU Instruction register. */
class FpuInstruction : public EnvReg {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Writes to an ostream using text. */
  std::ostream& write_text(std::ostream& os) const {
    return (os << "%instruction");
  }

private:
  /** Direct access to this constructor is disallowed. */
  constexpr FpuInstruction() : EnvReg(0) {}
};

/** The FPU Opcode regiter. */
class FpuOpcode : public EnvReg {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Writes to an ostream using text. */
  std::ostream& write_text(std::ostream& os) const {
    return (os << "%opcode");
  }

private:
  /** Direct access to this constructor is disallowed. */
  constexpr FpuOpcode() : EnvReg(0) {}
};

/** The instruction pointer register. */
class Rip : public EnvReg {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Writes to an ostream using text. */
  std::ostream& write_text(std::ostream& os) const {
    return (os << "%rip");
  }

private:
  /** Direct access to this constructor is disallowed. */
  constexpr Rip() : EnvReg(0) {}
};

} // namespace x64asm

#endif
