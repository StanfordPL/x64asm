#include <cstdlib>
#include <fstream>
#include <iostream>
#include <set>
#include <string>
#include <unistd.h>
#include <vector>

#include "include/x64asm.h"

using namespace std;
using namespace x64asm;

// Opcodes that are known not to parse when printed
set<Opcode> bad_parse_ {};

// Opcodes that are known not to parse using g++
set<Opcode> bad_asm_ {};

// Opcodes that are known to produce hex different from g++
set<Opcode> bad_hex_ {};

Opcode opcode() {
  const auto num_opcs = (size_t)XTEST + 1;
  return (Opcode)(rand() % num_opcs);
}

Hint hint() {
  return rand() % 2 ? taken : not_taken;
}

Label label() {
  return Label(".L0");
}

Imm8 imm8() {
  return Imm8(rand() % (0x1ull << 8));
}

Imm16 imm16() {
  return Imm16(rand() % (0x1ull << 16));
}

Imm32 imm32() {
  return Imm32(rand() % (0x1ull << 32));
}

Imm64 imm64() {
  uint64_t upper = (uint64_t)rand() << 32;
  uint64_t lower = rand();
  return Imm64(upper | lower);
}

Mm mm() {
  return mms[rand() % mms.size()];
}

Rh rh() {
  return rhs[rand() % rhs.size()];
}

R8 r8_() {
  return r8s[rand() % r8s.size()];
}

R16 r16() {
  return r16s[rand() % r16s.size()];
}

R32 r32() {
  return r32s[rand() % r32s.size()];
}

R64 r64() {
  return r64s[rand() % r64s.size()];
}

Rel8 rel8() {
  return Rel8(rand() % (0x1ull << 8));
}

Rel32 rel32() {
  return Rel32(rand() % (0x1ull << 32));
}

Sreg sreg() {
  return sregs[rand() % sregs.size()];
}

St st() {
  return sts[rand() % sts.size()];
}

Xmm xmm() {
  return xmms[rand() % xmms.size()];
}

Ymm ymm() {
  return ymms[rand() % ymms.size()];
}

Scale scale() {
  return (Scale)(rand() % 4);
}

M8 mem() {
  auto m = M8(sreg(), r64(), r64(), scale(), imm32());
  m.set_addr_or(rand() % 2);
  if (rand() % 2) {
    m.clear_seg();
  }
  if (rand() % 2) {
    m.clear_base();
  }
  if (rand() % 2) {
    m.clear_index();
  }
  if (m.contains_index() && m.get_index() == rsp) {
    m.clear_index();
  }

  return m;
}

Moffs8 moffs() {
  auto m = Moffs8(sreg(), imm64());
  if (rand() % 2) {
    m.clear_seg();
  }
  return m;
}

Instruction instruction() {
  Instruction instr(NOP);
  instr.set_opcode(opcode());
  for (size_t i = 0, ie = instr.arity(); i < ie; ++i) {
    switch (instr.type(i)) {
    case Type::HINT:
      instr.set_operand(i, hint());
      break;

    case Type::IMM_8:
      instr.set_operand(i, imm8());
      break;
    case Type::IMM_16:
      instr.set_operand(i, imm16());
      break;
    case Type::IMM_32:
      instr.set_operand(i, imm32());
      break;
    case Type::IMM_64:
      instr.set_operand(i, imm64());
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

    case Type::LABEL:
      instr.set_operand(i, label());
      break;

    case Type::M_8:
    case Type::M_16:
    case Type::M_32:
    case Type::M_64:
    case Type::M_128:
    case Type::M_256:
    case Type::M_16_INT:
    case Type::M_32_INT:
    case Type::M_64_INT:
    case Type::M_32_FP:
    case Type::M_64_FP:
    case Type::M_80_FP:
    case Type::M_80_BCD:
    case Type::M_2_BYTE:
    case Type::M_28_BYTE:
    case Type::M_108_BYTE:
    case Type::M_512_BYTE:
    case Type::FAR_PTR_16_16:
    case Type::FAR_PTR_16_32:
    case Type::FAR_PTR_16_64:
      instr.set_operand(i, mem());
      break;

    case Type::MM:
      instr.set_operand(i, mm());
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

    case Type::MOFFS_8:
    case Type::MOFFS_16:
    case Type::MOFFS_32:
    case Type::MOFFS_64:
      instr.set_operand(i, moffs());
      break;

    case Type::RH:
      instr.set_operand(i, rh());
      break;
    case Type::R_8:
      instr.set_operand(i, r8_());
      break;
    case Type::AL:
      instr.set_operand(i, al);
      break;
    case Type::CL:
      instr.set_operand(i, cl);
      break;
    case Type::R_16:
      instr.set_operand(i, r16());
      break;
    case Type::AX:
      instr.set_operand(i, ax);
      break;
    case Type::DX:
      instr.set_operand(i, dx);
      break;
    case Type::R_32:
      instr.set_operand(i, r32());
      break;
    case Type::EAX:
      instr.set_operand(i, eax);
      break;
    case Type::R_64:
      instr.set_operand(i, r64());
      break;
    case Type::RAX:
      instr.set_operand(i, rax);
      break;

    case Type::REL_8:
      instr.set_operand(i, rel8());
      break;
    case Type::REL_32:
      instr.set_operand(i, rel32());
      break;

    case Type::SREG:
      instr.set_operand(i, sreg());
      break;
    case Type::FS:
      instr.set_operand(i, fs);
      break;
    case Type::GS:
      instr.set_operand(i, gs);
      break;

    case Type::ST:
      instr.set_operand(i, st());
      break;
    case Type::ST_0:
      instr.set_operand(i, st0);
      break;

    case Type::XMM:
      instr.set_operand(i, xmm());
      break;
    case Type::XMM_0:
      instr.set_operand(i, xmm0);
      break;

    case Type::YMM:
      instr.set_operand(i, ymm());
      break;

    default:
      cout << "Control should never reach here!" << endl;
      exit(1);
    }
  }

  if (!instr.check()) {
    cout << "Generated ill-formed instruction: " << instr << endl;
    exit(1);
  }

  return instr;
}

string tempfile(const string& temp) {
  vector<char> v(temp.begin(), temp.end());
  v.push_back('\0');

  const auto fd = mkstemp(v.data());
  close(fd);
  return string(v.begin(), v.end()-1);
}

int main(int argc, char** argv) {
  srand(time(0));

  size_t itrs = 1;
  if (argc > 1) {
    itrs = atoi(argv[1]);
  }

  const auto known_bad_parse = bad_parse_.size();
  const auto known_bad_asm = bad_asm_.size();
  const auto known_bad_hex = bad_hex_.size();

  // Temp filenames
  auto s_file = tempfile("/tmp/x64asm_fuzz.s.XXXXXX");
  auto hex_file = tempfile("/tmp/x64asm_fuzz.hex.XXXXXX");
  auto o_file = tempfile("/tmp/x64asm_fuzz.o.XXXXXX");
  auto od_file = tempfile("/tmp/x64asm_fuzz.od.XXXXXX");

  for (size_t i = 0; i < itrs; ++i) {
    // Generate a random instruction
    const auto instr = instruction();
    const auto opcode = instr.get_opcode();

    // Write it to a temp file
    ofstream ofs(s_file);
    ofs << instr << endl;

    // Try reading the instruction back in
    const auto cmd1 = "cat " + s_file + " | ./bin/asm 2>/dev/null | sed 'N;s/\\n//' | sed 's/ *$//' > " + hex_file;
    const auto res1 = system(cmd1.c_str());
    if (res1 != 0 && (bad_parse_.find(opcode) == bad_parse_.end())) {
      bad_parse_.insert(opcode);
      cout << "Unable to parse using asm: (" << opcode << ") " << instr << endl;
      cout << endl;
    }
    if (res1 != 0) {
      continue;
    }

    // Try assembling using g++
    const auto cmd2 = "g++ -x assembler -c " + s_file + " -o " + o_file + " 2> /dev/null";
    const auto res2 = system(cmd2.c_str());
    if (res2 != 0 && (bad_asm_.find(opcode) == bad_asm_.end())) {
      bad_asm_.insert(opcode);
      cout << "Unable to assemble using g++: (" << opcode << ") " << instr << endl;
      cout << endl;
    }
    if (res2 != 0) {
      continue;
    }

    // Compare hex
    const auto cmd3 = "objdump -d " + o_file + " | tail -n+8 | cut -c 7-27 | sed 'N;s/\\n//' | sed 's/ *$//' > " + od_file;
    const auto res3 = system(cmd3.c_str());
    if (res3 != 0) {
      continue;
    }
    const auto cmd4 = "diff " + hex_file + " " + od_file + " > /dev/null";
    const auto res4 = system(cmd4.c_str());
    if (res4 != 0 && (bad_hex_.find(opcode) == bad_hex_.end())) {
      bad_hex_.insert(opcode);
      cout << "Hex disagreement: (" << opcode << ") " << instr << endl;

      cout << "  x64asm: ";
      cout.flush();
      const auto cmd5 = "cat " + hex_file;
      const auto res5 = system(cmd5.c_str());
      if(res5 != 0) {
        cout << "Error reading file." << endl;
      }

      cout << "  g++:    ";
      cout.flush();
      const auto cmd6 = "cat " + od_file;
      const auto res6 = system(cmd6.c_str());
      cout << endl;
      if(res6 != 0) {
        cout << "Error reading file." << endl;
      }

    }
  }

  const auto new_bad_parse = bad_parse_.size() - known_bad_parse;
  const auto new_bad_asm = bad_asm_.size() - known_bad_asm;
  const auto new_bad_hex = bad_hex_.size() - known_bad_hex;

  cout << "Parse Errors: " << endl;
  cout << "  " << known_bad_parse << " known" << endl;
  cout << "  " << new_bad_parse << " new" << endl;
  cout << "Assembler Errors: " << endl;
  cout << "  " << known_bad_asm << " known" << endl;
  cout << "  " << new_bad_asm << " new" << endl;
  cout << "Hex Errors: " << endl;
  cout << "  " << known_bad_hex << " known" << endl;
  cout << "  " << new_bad_hex << " new" << endl;
  cout << "Total: " << endl;
  cout << "  " << (known_bad_parse + known_bad_asm + known_bad_hex) << " known" << endl;
  cout << "  " << (new_bad_parse + new_bad_asm + new_bad_hex) << " new" << endl;

  return 0;
}
