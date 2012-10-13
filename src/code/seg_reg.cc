#include "src/code/seg_reg.h"

using namespace std;

namespace x64 {

const vector<Sreg> Sreg::domain_ {{
	0, 1, 2, 3, 4, 5, 
}};

const Sreg es = 0;
const Sreg cs = 1;
const Sreg ss = 2;
const Sreg ds = 3;
const Sreg fs = 4;
const Sreg gs = 5;
const Sreg sreg_null = 6;

} // namespace x64

