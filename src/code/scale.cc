#include "src/code/scale.h"

using namespace std;

namespace x64 {

const Scale times_1 = 0;
const Scale times_2 = 1;
const Scale times_4 = 2;
const Scale times_8 = 3;

const vector<Scale> domain_ {{
	times_1,
	times_2,
	times_4,
	times_8
}};

} // namespace x64

