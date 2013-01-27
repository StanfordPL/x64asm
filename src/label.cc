#include "src/label.h"

using namespace std;

namespace x64asm {

OpType Label::type() const {
	return OpType::LABEL;
}

void Label::write_att(ostream& os) const {
	os << ".LABEL_" << dec << val();
}

void Label::write_intel(ostream& os) const {
}

} // namespace x64asm
