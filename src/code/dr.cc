#include "src/code/dr.h"

#include <cassert>

using namespace std;

namespace x64 {

OpType Dr::type() const {
	return OpType::DR;
}

bool Dr::check() const {
	return val() < 8;
}

void Dr::write_att(ostream& os) const {
	assert(val() < 8);
	os << "%dr" << dec << val();
}

void Dr::write_intel(ostream& os) const {
}

} // namespace x64
