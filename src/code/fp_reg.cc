#include "src/code/fp_reg.h"

using namespace std;

namespace x64 {

const FpReg st0 = 0;
const FpReg st1 = 1;
const FpReg st2 = 2;
const FpReg st3 = 3;
const FpReg st4 = 4;
const FpReg st5 = 5;
const FpReg st6 = 6;
const FpReg st7 = 7;
const FpReg fp_null = 8;

const std::vector<FpReg> FpReg::domain_ {{ 
	st0, 
	st1, 
	st2, 
	st3, 
	st4, 
	st5, 
	st6, 
	st7 
}};

} // namespace x64

