
#include "src/operand.h"

using namespace x64asm;

uint16_t Operand::size() const {

  uint16_t ret = 0;
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
      ret = 8;
      break;

    case Type::IMM_16:
    case Type::M_16:
    case Type::R_16:
    case Type::AX:
    case Type::DX:
      ret = 16;
      break;

    case Type::IMM_32:
    case Type::M_32:
    case Type::R_32:
    case Type::EAX:
      ret = 32;
      break;

    case Type::IMM_64:
    case Type::M_64:
    case Type::R_64:
    case Type::RAX:
      ret = 64;
      break;

    case Type::XMM:
    case Type::XMM_0:
      ret = 128;
      break;

    case Type::YMM:
      ret = 256;
      break;

    default:
      ret = 0;
      break;
  }

  return ret;
}

bool Operand::is_gp_register() const {

  bool b = false;
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
      b = true;
      break;

    default:
      b = false;
      break;
  }

  return b;
}

bool Operand::is_sse_register() const {
  return type() == Type::XMM || type() == Type::YMM;
}

bool Operand::is_typical_memory() const {

  bool b = false;
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
      b = true;
      break;

    default:
      b = false;
      break;
  }

  return b;
}

bool Operand::is_immediate() const {

  bool b = false;
  switch(type()) {
    case Type::IMM_8:
    case Type::IMM_16:
    case Type::IMM_32:
    case Type::IMM_64:
    case Type::ZERO:
    case Type::ONE:
    case Type::THREE:
      b = true;
      break;

    default:
      b = false;
      break;
  }

  return b;
}


