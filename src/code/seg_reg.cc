#include "src/code/seg_reg.h"

using namespace std;

namespace x64 {

const SegReg es = 0;
const SegReg cs = 1;
const SegReg ss = 2;
const SegReg ds = 3;
const SegReg fs = 4;
const SegReg gs = 5;
const SegReg seg_null = 6;

const vector<SegReg> SegReg::domain_ {{
	es,
	cs,
	ss,
	ds,
	fs,
	gs
}};

} // namespace x64

