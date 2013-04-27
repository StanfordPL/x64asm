#include "state.h"

#include <iomanip>
#include <string>
#include "include/x64asm.h"

using namespace std;
using namespace trace;
using namespace x64asm;

namespace {

void read_8(istream& is, uint8_t& x) {
  x = 0;

  string s;
  is >> s;

  if (s.length() != 8) {
    is.setstate(ios::failbit);
    return;
  }

  for (int i = 7; i >= 0; --i)
    if (s[7 - i] == '1') {
      x |= (0x1 << i);
    } else if (s[7 - i] != '0') {
      is.setstate(ios::failbit);
      return;
    }
}

void read_64(istream& is, uint64_t& x) {
  x = 0;

  for (int i = 7; i >= 0; --i) {
    uint8_t byte;
    read_8(is, byte);

    x |= (((uint64_t)byte) << (i * 8));
  }
}

void read_128(istream& is, __uint128_t& x) {
  x = 0;

  uint64_t quad;
  read_64(is, quad);
  x |= (((__uint128_t) quad) << 64);
  read_64(is, quad);
  x |= (__uint128_t) quad;
}

void read_gp(istream& is, const string& gp, uint64_t& val) {
  string s;
  is >> s;

  if (s != gp) {
    is.setstate(ios::failbit);
    return;
  }

  read_64(is, val);
}

void read_xmm(istream& is, const string& xmm, __uint128_t& val) {
  string s;
  is >> s;

  if (s != xmm) {
    is.setstate(ios::failbit);
    return;
  }

  read_128(is, val);
}

void write_8(ostream& os, uint8_t x) {
  for (int i = 7; i >= 0; --i) {
    const auto bit = (x >> i) & 0x01;
    os << (bit > 0 ? "1" : "0");
  }
}

void write_64(ostream& os, uint64_t x) {
  for (int i = 7; i >= 0; --i) {
    const auto byte = (x >> (i * 8)) & 0xff;
    write_8(os, byte);

    if (i != 0) {
      os << " ";
    }
  }
}

void write_128(ostream& os, __uint128_t x) {
  const auto upper = x >> 64;
  write_64(os, upper);
  os << " ";
  const auto lower = x & 0xffffffffffffffff;
  write_64(os, lower);
}

void write_gp(ostream& os, const string& gp, uint64_t val) {
  os << setw(5) << gp << " ";
  write_64(os, val);
}

void write_xmm(ostream& os, const string& xmm, __uint128_t val) {
  os << setw(5) << xmm << " ";
  write_128(os, val);
}

} // namespace

istream& operator>>(istream& is, State& s) {
  read_gp(is, "rax", s.general_purpose[rax]);
  read_gp(is, "rbx", s.general_purpose[rbx]);
  read_gp(is, "rcx", s.general_purpose[rcx]);
  read_gp(is, "rdx", s.general_purpose[rdx]);
  read_gp(is, "rdi", s.general_purpose[rdi]);
  read_gp(is, "rsi", s.general_purpose[rsi]);
  read_gp(is, "rbp", s.general_purpose[rbp]);
  read_gp(is, "rsp", s.general_purpose[rsp]);
  read_gp(is, "r8",  s.general_purpose[r8]);
  read_gp(is, "r9",  s.general_purpose[r9]);
  read_gp(is, "r10", s.general_purpose[r10]);
  read_gp(is, "r11", s.general_purpose[r11]);
  read_gp(is, "r12", s.general_purpose[r12]);
  read_gp(is, "r13", s.general_purpose[r13]);
  read_gp(is, "r14", s.general_purpose[r14]);
  read_gp(is, "r15", s.general_purpose[r15]);

  read_xmm(is, "xmm0",  s.xmm[xmm0]);
  read_xmm(is, "xmm1",  s.xmm[xmm1]);
  read_xmm(is, "xmm2",  s.xmm[xmm2]);
  read_xmm(is, "xmm3",  s.xmm[xmm3]);
  read_xmm(is, "xmm4",  s.xmm[xmm4]);
  read_xmm(is, "xmm5",  s.xmm[xmm5]);
  read_xmm(is, "xmm6",  s.xmm[xmm6]);
  read_xmm(is, "xmm7",  s.xmm[xmm7]);
  read_xmm(is, "xmm8",  s.xmm[xmm8]);
  read_xmm(is, "xmm9",  s.xmm[xmm9]);
  read_xmm(is, "xmm10", s.xmm[xmm10]);
  read_xmm(is, "xmm11", s.xmm[xmm11]);
  read_xmm(is, "xmm12", s.xmm[xmm12]);
  read_xmm(is, "xmm13", s.xmm[xmm13]);
  read_xmm(is, "xmm14", s.xmm[xmm14]);
  read_xmm(is, "xmm15", s.xmm[xmm15]);

  return is;
}

ostream& operator<<(ostream& os, const State& s) {
  write_gp(os, "rax", s.general_purpose[rax]);
  os << endl;
  write_gp(os, "rbx", s.general_purpose[rbx]);
  os << endl;
  write_gp(os, "rcx", s.general_purpose[rcx]);
  os << endl;
  write_gp(os, "rdx", s.general_purpose[rdx]);
  os << endl;
  write_gp(os, "rdi", s.general_purpose[rdi]);
  os << endl;
  write_gp(os, "rsi", s.general_purpose[rsi]);
  os << endl;
  write_gp(os, "rbp", s.general_purpose[rbp]);
  os << endl;
  write_gp(os, "rsp", s.general_purpose[rsp]);
  os << endl;
  write_gp(os, "r8",  s.general_purpose[r8]);
  os << endl;
  write_gp(os, "r9",  s.general_purpose[r9]);
  os << endl;
  write_gp(os, "r10", s.general_purpose[r10]);
  os << endl;
  write_gp(os, "r11", s.general_purpose[r11]);
  os << endl;
  write_gp(os, "r12", s.general_purpose[r12]);
  os << endl;
  write_gp(os, "r13", s.general_purpose[r13]);
  os << endl;
  write_gp(os, "r14", s.general_purpose[r14]);
  os << endl;
  write_gp(os, "r15", s.general_purpose[r15]);
  os << endl;

  write_xmm(os, "xmm0",  s.xmm[xmm0]);
  os << endl;
  write_xmm(os, "xmm1",  s.xmm[xmm1]);
  os << endl;
  write_xmm(os, "xmm2",  s.xmm[xmm2]);
  os << endl;
  write_xmm(os, "xmm3",  s.xmm[xmm3]);
  os << endl;
  write_xmm(os, "xmm4",  s.xmm[xmm4]);
  os << endl;
  write_xmm(os, "xmm5",  s.xmm[xmm5]);
  os << endl;
  write_xmm(os, "xmm6",  s.xmm[xmm6]);
  os << endl;
  write_xmm(os, "xmm7",  s.xmm[xmm7]);
  os << endl;
  write_xmm(os, "xmm8",  s.xmm[xmm8]);
  os << endl;
  write_xmm(os, "xmm9",  s.xmm[xmm9]);
  os << endl;
  write_xmm(os, "xmm10", s.xmm[xmm10]);
  os << endl;
  write_xmm(os, "xmm11", s.xmm[xmm11]);
  os << endl;
  write_xmm(os, "xmm12", s.xmm[xmm12]);
  os << endl;
  write_xmm(os, "xmm13", s.xmm[xmm13]);
  os << endl;
  write_xmm(os, "xmm14", s.xmm[xmm14]);
  os << endl;
  write_xmm(os, "xmm15", s.xmm[xmm15]);

  return os;
}
