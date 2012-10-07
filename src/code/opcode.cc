#include "src/code/opcode.h"

using namespace std;
using namespace x64;

namespace x64 { 

// Static array declarations
#include "src/gen/opcode.static"

void Opcode::write_att(ostream& os) const {
	if ( is_label_defn() )
		os.setstate(ios::failbit);
	else {
		assert(o_ < ::opcodes_.size());
		os << ::opcodes_[o_];
	}
}

} // namespace x64
