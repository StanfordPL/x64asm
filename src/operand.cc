
#include "src/operand.h"

using namespace x64asm;

uint16_t Operand::size() const {

  switch(type()) {
    case Type::IMM_8:
    case Type::ZERO:
    case Type::ONE:
    case Type::THREE:
    case Type::M_8:
    case Type::MOFFS_8:
    case Type::RL:
    case Type::RB:
    case Type::RH:
    case Type::AL:
    case Type::CL:
      return 8;

    case Type::IMM_16:
    case Type::M_16:
    case Type::R_16:
    case Type::MOFFS_16:
    case Type::AX:
    case Type::DX:
      return 16;

    case Type::IMM_32:
    case Type::MOFFS_32:
    case Type::M_32:
    case Type::R_32:
    case Type::EAX:
      return 32;

    case Type::IMM_64:
    case Type::MOFFS_64:
    case Type::M_64:
    case Type::R_64:
    case Type::RAX:
      return 64;

    case Type::XMM:
    case Type::XMM_0:
    case Type::M_128:
      return 128;

    case Type::YMM:
    case Type::M_256:
      return 256;

    default:
      return 0;
  }
}

bool Operand::is_gp_register() const {

  switch(type()) {
    case Type::RL:
    case Type::RH:
    case Type::RB:
    case Type::AL:
    case Type::CL:
    case Type::R_16:
    case Type::AX:
    case Type::DX:
    case Type::R_32:
    case Type::EAX:
    case Type::R_64:
    case Type::RAX:
      return true;

    default:
      return false;
  }
}

bool Operand::is_sse_register() const {
  return type() == Type::XMM || type() == Type::YMM ||
         type() == Type::XMM_0;
}

bool Operand::is_typical_memory() const {

  switch(type()) {
    case Type::M_8:
    case Type::M_16:
    case Type::M_32:
    case Type::M_64:
    case Type::M_128:
    case Type::M_256:
    case Type::MOFFS_8:
    case Type::MOFFS_16:
    case Type::MOFFS_32:
    case Type::MOFFS_64:
      return true;

    default:
      return false;
  }
}

bool Operand::is_immediate() const {

  switch(type()) {
    case Type::IMM_8:
    case Type::IMM_16:
    case Type::IMM_32:
    case Type::IMM_64:
    case Type::ZERO:
    case Type::ONE:
    case Type::THREE:
      return true;

    default:
      return false;
  }

}


