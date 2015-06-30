#include <chrono>
#include <iostream>
#include <stdint.h>

#include "include/x64asm.h"

using namespace std;
using namespace std::chrono;
using namespace x64asm;

char garbage_buffer[0x10000] __attribute__((aligned(0x100)));

/** Constant: The number of iterations to run a function for */
constexpr size_t iterations() {
  return 100000;
}

constexpr size_t instr_count() {
  return 1000;
}

/** Time the execution of a function call in 0.01 * nanoseconds */
time_t measure(const Function& fxn) {
  const auto begin = high_resolution_clock::now();
  for (size_t i = 0; i < iterations(); ++i) {
    fxn.call<int, char*>(garbage_buffer, garbage_buffer, garbage_buffer);
  }
  const auto end = high_resolution_clock::now();
  return 100*duration_cast<nanoseconds>(end-begin).count() / iterations();
}

/** Time a function that contains a single instruction */
time_t measure_instruction(Function& fxn, const Instruction& instr) {
  Assembler assm;
  assm.start(fxn);
  for(size_t i = 0; i < instr_count(); ++i)
    assm.assemble(instr);
  assm.ret();
  assm.finish();

  return measure(fxn);
}

/** Time a function of nops */
time_t measure_baseline(Function& fxn) {
  auto instr = Instruction(NOP);
  return measure_instruction(fxn, instr);
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
  return 
      // These can access memory at an index, and cause a segfault
      instr.get_opcode() == Opcode::BT_M32_R32 ||
      instr.get_opcode() == Opcode::BT_M64_R64 ||
      instr.get_opcode() == Opcode::BTC_M32_R32 ||
      instr.get_opcode() == Opcode::BTC_M64_R64 ||
      instr.get_opcode() == Opcode::BTR_M32_R32 ||
      instr.get_opcode() == Opcode::BTR_M64_R64 ||
      instr.get_opcode() == Opcode::BTS_M32_R32 ||
      instr.get_opcode() == Opcode::BTS_M64_R64 ||

      // FPE
      instr.get_opcode() == Opcode::FLDCW_M2BYTE ||
      instr.get_opcode() == Opcode::FLDL2E ||
      instr.get_opcode() == Opcode::FLDL2T ||
      instr.get_opcode() == Opcode::FLDLG2 ||
      instr.get_opcode() == Opcode::FLDLN2 ||
      instr.get_opcode() == Opcode::FLDPI ||
      instr.get_opcode() == Opcode::FLDZ ||
      instr.get_opcode() == Opcode::FMUL_M32FP ||
      instr.get_opcode() == Opcode::FMUL_M64FP ||
      instr.get_opcode() == Opcode::FMUL_ST_ST0 ||
      instr.get_opcode() == Opcode::FMUL_ST0_ST ||
      instr.get_opcode() == Opcode::FMULP ||
      instr.get_opcode() == Opcode::FMULP_ST_ST0 ||

      // May be fixable
      instr.get_opcode() == Opcode::FXRSTOR_M512BYTE ||
      instr.get_opcode() == Opcode::FXRSTOR64_M512BYTE ||

      // This is a ring 0 instruction...
      instr.get_opcode() == Opcode::INVPCID_R64_M128 ||

      // Messes with MXCSR
      instr.get_opcode() == Opcode::LDMXCSR_M32 ||

      // Not allowed in 64-bit mode??
      instr.get_opcode() == Opcode::LFS_R16_FARPTR1616 ||
      instr.get_opcode() == Opcode::LFS_R32_FARPTR1632 ||
      instr.get_opcode() == Opcode::LFS_R64_FARPTR1664 ||
      instr.get_opcode() == Opcode::LGS_R16_FARPTR1616 ||
      instr.get_opcode() == Opcode::LGS_R32_FARPTR1632 ||
      instr.get_opcode() == Opcode::LGS_R64_FARPTR1664 ||
      instr.get_opcode() == Opcode::LSS_R16_FARPTR1616 ||
      instr.get_opcode() == Opcode::LSS_R32_FARPTR1632 ||
      instr.get_opcode() == Opcode::LSS_R64_FARPTR1664 ||

      // REP
      (instr.get_opcode() >= Opcode::REP_INS_M16_DX &&
       instr.get_opcode() <= Opcode::REPNE_SCAS_M8_1) ||

      // Illegal instruction !!! BUG
      instr.get_opcode() == Opcode::VGATHERDPD_XMM_M32_XMM ||
      instr.get_opcode() == Opcode::VGATHERDPD_YMM_M32_YMM ||
      instr.get_opcode() == Opcode::VGATHERDPS_XMM_M32_XMM ||
      instr.get_opcode() == Opcode::VGATHERDPS_YMM_M32_YMM ||
      instr.get_opcode() == Opcode::VGATHERQPD_XMM_M64_XMM ||
      instr.get_opcode() == Opcode::VGATHERQPD_YMM_M64_YMM ||
      instr.get_opcode() == Opcode::VGATHERQPS_XMM_M64_XMM ||
      instr.get_opcode() == Opcode::VGATHERQPS_XMM_M64_XMM_1 ||

      //FPE
      (instr.get_opcode() >= Opcode::VMULPD_XMM_XMM_M128 &&
      instr.get_opcode() <= Opcode::VMULSS_XMM_XMM_XMM) ||

      // Illegal instruction !!! BUG
       (instr.get_opcode() >= Opcode::VPGATHERDD_XMM_M32_XMM &&
        instr.get_opcode() <= Opcode::VPGATHERQQ_YMM_M64_YMM) ||

      //FPE
      (instr.get_opcode() >= Opcode::VROUNDPD_XMM_M128_IMM8 &&
       instr.get_opcode() <= Opcode::VROUNDSS_XMM_XMM_XMM_IMM8) ||
      (instr.get_opcode() >= Opcode::VSQRTPD_XMM_M128 &&
       instr.get_opcode() <= Opcode::VSQRTSS_XMM_XMM_XMM) ||

      //stupid
      instr.get_opcode() == Opcode::XACQUIRE ||

      //segfault
      instr.get_opcode() == Opcode::XLAT_M8 ||
      instr.get_opcode() == Opcode::XLATB ||
      instr.get_opcode() == Opcode::XLATB_1 ||

      //stupid
      instr.get_opcode() >= Opcode::XRELEASE ||


      // From Eric
      instr.get_opcode() == Opcode::CLI || 
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
      instr.is_implicit_memory_dereference();
}

/** Returns a valid instance of this instruction */
Instruction get_instruction(Opcode o, bool& ok) {
  ok = true;
  Instruction instr(o);
  if (is_control(instr) || is_problematic(instr)) {
    return instr;
  }

  for (size_t i = 0, ie = instr.arity(); i < ie; ++i) {
    switch(instr.type(i)) {
      case Type::IMM_8:
        instr.set_operand(i, Imm8(0x4));
        break;
      case Type::IMM_16:
        instr.set_operand(i, Imm16(0x4050));
        break;
      case Type::IMM_32:
        instr.set_operand(i, Imm32(0xc0decafe));
        break;
      case Type::IMM_64:
        instr.set_operand(i, Imm64(0xbadeb0dec0decafe));
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

      case Type::M_8:
        instr.set_operand(i, M8(rsi));
        break;
      case Type::M_16:
      case Type::M_16_INT:
        instr.set_operand(i, M16(rsi));
        break;
      case Type::M_32:
      case Type::M_32_INT:
      case Type::M_32_FP:
        instr.set_operand(i, M32(rsi));
        break;
      case Type::M_64:
      case Type::M_64_INT:
      case Type::M_64_FP:
        instr.set_operand(i, M64(rsi));
        break;
      case Type::M_80_BCD:
      case Type::M_80_FP:
        instr.set_operand(i, M80Fp(rsi));
        break;
      case Type::M_128:
        instr.set_operand(i, M128(rsi));
        break;
      case Type::M_256:
        instr.set_operand(i, M256(rsi));
        break;
      case Type::M_2_BYTE:
        instr.set_operand(i, M2Byte(rsi));
        break;
      case Type::M_28_BYTE:
        instr.set_operand(i, M28Byte(rsi));
        break;
      case Type::M_108_BYTE:
        instr.set_operand(i, M108Byte(rsi));
        break;
      case Type::M_512_BYTE:
        instr.set_operand(i, M512Byte(rsi));
        break;
      case Type::FAR_PTR_16_16:
        instr.set_operand(i, FarPtr1616(rsi));
        break;
      case Type::FAR_PTR_16_32:
        instr.set_operand(i, FarPtr1632(rsi));
        break;
      case Type::FAR_PTR_16_64:
        instr.set_operand(i, FarPtr1664(rsi));
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
        instr.set_operand(i, ch);
        break;
      case Type::AL:
        instr.set_operand(i, al);
        break;
      case Type::R_8:
        instr.set_operand(i, dl);
        break;
      case Type::CL:
        instr.set_operand(i, cl);
        break;
      case Type::R_16:
        instr.set_operand(i, cx);
        break;
      case Type::AX:
        instr.set_operand(i, ax);
        break;
      case Type::DX:
        instr.set_operand(i, dx);
        break;
      case Type::EAX:
        instr.set_operand(i, eax);
        break;
      case Type::R_32:
        instr.set_operand(i, edx);
        break;
      case Type::RAX:
        instr.set_operand(i, rax);
        break;
      case Type::R_64:
        instr.set_operand(i, rdx);
        break;

        /*
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
        */

      case Type::XMM:
      case Type::XMM_0:
        instr.set_operand(i, xmm0);
        break;

      case Type::YMM:
        instr.set_operand(i, ymm0);
        break;

      default:
        ok = false;
        break;
    }
  }

  return instr;
}

/** Estimates the runtime of an instruction in 0.1xnano-seconds (rounded up) */
time_t get_latency(Function& fxn, const Instruction& instr, time_t baseline, bool ok) {
  if (is_control(instr)) {
    return 0;
  } else if (is_problematic(instr) || !ok) {
    return 999;
  } else {
    const auto m = measure_instruction(fxn, instr);
    return m > baseline ? (m - baseline)/instr_count() : 1;
  }
}

int main() {
  Function fxn;
  fxn.reserve(32*1200);
  const auto baseline = measure_baseline(fxn);

  for (size_t i = 1; i < NUM_OPCODES; ++i) {
    const auto opc = (Opcode)i;
    bool ok = true;
    const auto instr = get_instruction(opc, ok);
    const auto latency = get_latency(fxn, instr, baseline, ok);

    cout << ", " << latency << "\t\t// " << instr << endl;
  }

  return 0;
}
