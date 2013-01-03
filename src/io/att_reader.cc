#include "src/io/att_reader.h"

using namespace std;

namespace x64 {

void AttReader::read(istream& is, Code& code) {
	/* TODO...
	stringstream ss;
	ss << is.rdbuf();

	auto buffer = att_scan_string(ss.str().c_str());
	att_switch_to_buffer(buffer);
	attparse(is, code);
	att_delete_buffer(buffer);
	*/
}

} // namespace x64
