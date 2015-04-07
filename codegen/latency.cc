#include <chrono>
#include <iostream>
#include <stdint.h>

#include "include/x64asm.h"

using namespace std;
using namespace std::chrono;
using namespace x64asm;

/** Constant: The number of iterations to run a function for */
constexpr size_t iterations() {
  return 1000000;
}

/** Time the execution of a function call in 0.01 * nanoseconds */
time_t measure(const Function& fxn) {
  const auto begin = high_resolution_clock::now();
  for (size_t i = 0; i < iterations(); ++i) {
    fxn.call<int>();
  }
  const auto end = high_resolution_clock::now();
  return 100*duration_cast<nanoseconds>(end-begin).count() / iterations();
}

/** Time an empty function */
time_t measure_baseline(Function& fxn) {
  Assembler assm;
  assm.start(fxn);
  assm.ret();
  assm.finish();

  return measure(fxn); 
}

/** Time a function that contains a single instruction */
time_t measure_instruction(Function& fxn, const Instruction& instr) {
  Assembler assm;
  assm.start(fxn);
  assm.assemble(instr);
  assm.ret();
  assm.finish();

  return measure(fxn);
}

/** Returns true if this is an instruction that induces control flow */
bool is_control(const Instruction& instr) {
  return instr.is_label_defn() || instr.is_any_jump() || 
    instr.is_any_call() || instr.is_any_return() ||
    instr.is_enter() || instr.is_leave() ||
    instr.is_any_loop();
}

/** Returns true if simulating this instruction is problematic */
bool is_problematic(const Instruction& instr) {
  return instr.get_opcode() == Opcode::CLI || 
      instr.get_opcode() == Opcode::MONITOR ||
      instr.get_opcode() == Opcode::MOV_SREG_R16 ||
      instr.get_opcode() == Opcode::MOV_SREG_R64 ||
      instr.get_opcode() == Opcode::MWAIT ||
      instr.get_opcode() == Opcode::STD ||
      instr.get_opcode() == Opcode::STI ||
      instr.get_opcode() == Opcode::SWAPGS ||
      instr.get_opcode() == Opcode::UD2 ||
      instr.get_opcode() == Opcode::XABORT_IMM8 ||
      instr.get_opcode() == Opcode::XEND ||
      instr.get_opcode() == Opcode::XGETBV ||
      instr.get_opcode() == Opcode::XLATB ||
      instr.get_opcode() == Opcode::XTEST ||
      instr.is_any_string() ||
      instr.is_div() || instr.is_idiv() || 
      instr.is_in() || instr.is_out() ||
      instr.is_rdfsbase() || instr.is_rdgsbase() ||
      instr.is_sysenter() || instr.is_sysexit() || instr.is_sysret() ||
      instr.is_int() ||
      instr.is_lock() ||
      instr.is_wrfsbase() || instr.is_wrgsbase() ||
      instr.is_xbegin() ||
      instr.is_memory_dereference();
}

/** Returns a valid instance of this instruction */
Instruction get_instruction(Opcode o) {
  Instruction instr(o);
  if (is_control(instr) || is_problematic(instr)) {
    return instr;
  }

  for (size_t i = 0, ie = instr.arity(); i < ie; ++i) {
    switch(instr.type(i)) {
      case Type::IMM_8:
        instr.set_operand(i, Imm8(0));
        break;
      case Type::IMM_16:
        instr.set_operand(i, Imm16(0));
        break;
      case Type::IMM_32:
        instr.set_operand(i, Imm32(0));
        break;
      case Type::IMM_64:
        instr.set_operand(i, Imm64(0));
        break;
      case Type::ZERO:
        instr.set_operand(i, zero);
        break;
      case Type::ONE:
        instr.set_operand(i, one);
        break;
      case Type::THREE:
        instr.set_operand(i, three);
        break;

      case Type::M_16:
        instr.set_operand(i, M16(rax));
        break;
      case Type::M_32:
        instr.set_operand(i, M32(rax));
        break;
      case Type::M_64:
        instr.set_operand(i, M64(rax));
        break;

      case Type::MM:
        instr.set_operand(i, mm0);
        break;

      case Type::PREF_66:
        instr.set_operand(i, pref_66);
        break;
      case Type::PREF_REX_W:
        instr.set_operand(i, pref_rex_w);
        break;
      case Type::FAR:
        instr.set_operand(i, far);
        break;

      case Type::RH:
        instr.set_operand(i, ah);
        break;
      case Type::R_8:
      case Type::AL:
        instr.set_operand(i, al);
        break;
      case Type::CL:
        instr.set_operand(i, cl);
        break;
      case Type::R_16:
      case Type::AX:
        instr.set_operand(i, ax);
        break;
      case Type::DX:
        instr.set_operand(i, dx);
        break;
      case Type::R_32:
      case Type::EAX:
        instr.set_operand(i, eax);
        break;
      case Type::R_64:
      case Type::RAX:
        instr.set_operand(i, rax);
        break;

      case Type::SREG:
      case Type::FS:
        instr.set_operand(i, fs);
        break;
      case Type::GS:
        instr.set_operand(i, gs);
        break;

      case Type::ST:
      case Type::ST_0:
        instr.set_operand(i, st0);
        break;

      case Type::XMM:
      case Type::XMM_0:
        instr.set_operand(i, xmm0);
        break;

      case Type::YMM:
        instr.set_operand(i, ymm0);
        break;

      default:
        cout << "Control should never reach here!" << endl;
        cout << "INSTR = " << instr << " IDX = " << i << endl;
        exit(1);
    }
  }

  return instr;
}

/** Estimates the runtime of an instruction in 0.1xnano-seconds (rounded up) */
time_t get_latency(Function& fxn, const Instruction& instr, time_t baseline) {
  if (is_control(instr)) {
    return 0;
  } else if (is_problematic(instr)) {
    return 999;
  } else {
    const auto m = measure_instruction(fxn, instr);
    return m > baseline ? (m - baseline) : 1;
  }
}

int main() {
  Function fxn;
  const auto baseline = measure_baseline(fxn);

  for (size_t i = 1; i < NUM_OPCODES; ++i) {
    const auto opc = (Opcode)i;
    const auto instr = get_instruction(opc);
    const auto latency = get_latency(fxn, instr, baseline);

    cout << ", " << latency << "\t\t// " << instr << endl;
  }

  return 0;
}
