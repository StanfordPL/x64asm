#include "src/code/attributes.h"

using namespace std;

namespace x64 {

OpSet Attributes::explicit_read_set(const Instruction& i) {
	// TODO
	return OpSet::empty();
}

OpSet Attributes::explicit_write_set(const Instruction& i) {
	// TODO
	return OpSet::empty();
}

OpSet Attributes::explicit_def_set(const Instruction& i) {
	// TODO
	return OpSet::empty();
}

OpSet Attributes::explicit_undef_set(const Instruction& i) {
	// TODO
	return OpSet::empty();
}

vector<size_t> Attributes::arity_ {{
	// Internal mnemonics
	0
	// Auto-generated mnemonics
	#include "src/code/arity.table"
}};

vector<vector<OpAccessor>> Attributes::accessor_ {{
	// Internal mnemonics
	{{}}
	// Auto-generated mnemonics
	#include "src/code/accessor.table"
}};

vector<vector<OpType>> Attributes::type_ {{
	// Internal mnemonics
	{{}}
	// Auto-generated mnemonics
	#include "src/code/type.table"
}};

vector<bool> Attributes::is_return_ {{
	// Internal mnemonics
	false
	// Auto-generated mnemonics
	#include "src/code/return.table"
}};

vector<bool> Attributes::is_jump_ {{
	// Internal mnemonics
	false
	// Auto-generated mnemonics
	#include "src/code/jump.table"
}};

vector<bool> Attributes::is_cond_jump_ {{
	// Internal mnemonics
	false
	// Auto-generated mnemonics
	#include "src/code/cond_jump.table"
}};

vector<bool> Attributes::is_uncond_jump_ {{
	// Internal mnemonics
	false
	// Auto-generated mnemonics
	#include "src/code/uncond_jump.table"
}};

vector<OpSet> Attributes::implicit_read_set_ {{
	// Internal mnemonics
	OpSet::empty()
	// Auto-generated mnemonics
	#include "src/code/read.table"
}};

vector<OpSet> Attributes::implicit_write_set_ {{
	// Internal mnemonics
	OpSet::empty()
	// Auto-generated mnemonics
	#include "src/code/write.table"
}};

vector<OpSet> Attributes::implicit_def_set_ {{
	// Internal mnemonics
	OpSet::empty()
	// Auto-generated mnemonics
	#include "src/code/def.table"
}};

vector<OpSet> Attributes::implicit_undef_set_ {{
	// Internal mnemonics
	OpSet::empty()
	// Auto-generated mnemonics
	#include "src/code/undef.table"
}};

} // namespace x64
