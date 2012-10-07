#include "src/code/code.h"

#include <sstream>

#include "src/gen/att.tab.c"
#include "src/gen/lex.att.c"

using namespace std;

namespace x64 {

void Code::read_att(istream& is) {
	stringstream ss;
	ss << is.rdbuf();

	auto buffer = att_scan_string(ss.str().c_str());
	att_switch_to_buffer(buffer);
	attparse(is, *this);
	att_delete_buffer(buffer);
}

} // namespace x64
