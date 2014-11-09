
#include "src/operand.h"

uint16_t Operand::size() {

  switch(type()) {
    case IMM_8:
    case ZERO:
    case ONE:
    case THREE:
    case M_8:
    case MOFFS_8:
    case RL:
    case RB:
    case RH:
    case AL:
    case CL:
      return 8;

    case IMM_16:
    case M_16:
    case R_16:
    case AX:
    case DX:
      return 16;

    case IMM_32:
    case M_32:
    case R_32:
    case EAX:
      return 32;

    case IMM_64:
    case M_64:
    case R_64:
    case RAX:
      return 64;

    case XMM:
    case XMM_0:
      return 128;

    case YMM:
      return 256;

    default:
      return 0;
  }
}

bool Operand::is_gp_register() {

  switch(type()) {
    case RL:
    case RH:
    case RB:
    case AL:
    case CL:
    case R_16:
    case AX:
    case DX:
    case R_32:
    case EAX:
    case R_64:
    case RAX:
      return true;

    default:
      return false;
  }
}

bool Operand::is_sse_register() {
  return type() == XMM || type() == YMM;
}

bool Operand::is_typical_memory() {

  switch(type()) {
    case M_8:
    case M_16:
    case M_32:
    case M_64:
    case M_128:
    case M_256:
    case MOFFS_8:
    case MOFFS_16:
    case MOFFS_32:
    case MOFFS_64:
      return true;

    default:
      return false;
  }

}

bool Operand::is_immediate() {

  switch(type()) {
    case IMM_8:
    case IMM_16:
    case IMM_32:
    case IMM_64:
    case ZERO:
    case ONE:
    case THREE:
      return true;

    default:
      return false;
  }
}


