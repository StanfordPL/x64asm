#include "src/att/att_reader.h"

#include <sstream>

#include "src/gen/att.tab.c"
#include "src/gen/lex.att.c"

using namespace std;

namespace x64 {

void AttReader::read(istream& is, Code& code) const {
	stringstream ss;
	ss << is.rdbuf();

	auto buffer = att_scan_string(ss.str().c_str());
	att_switch_to_buffer(buffer);
	attparse(is, code);
	att_delete_buffer(buffer);
}

} // namespace x64
