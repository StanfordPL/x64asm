#include "src/code/sreg.h"

using namespace std;

namespace x64 {

const Sreg es{0};
const Sreg cs{1};
const Sreg ss{2};
const Sreg ds{3};
const Fs fs;
const Gs gs;

const Sregs sregs {{
	es, cs, ss, ds, fs, gs
}};

} // namespace x64

