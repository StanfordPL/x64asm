#include "src/code/cond_reg.h"

using namespace std;

namespace x64 {

const CondReg af = 0;
const CondReg cf = 1;
const CondReg of = 2;
const CondReg pf = 3;
const CondReg sf = 4;
const CondReg zf = 5;
const CondReg cond_null = 6;

const vector<CondReg> CondReg::domain_ {{ 
	af, 
	cf, 
	of, 
	pf, 
	sf, 
	zf 
}};

} // namespace x64
