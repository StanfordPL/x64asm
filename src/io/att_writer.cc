#include "src/io/att_writer.h"

#include <cassert>
#include <vector>

using namespace std;

namespace x64 {

void AttWriter::write(ostream& os, const Code& c) {
	for ( const auto& instr : c ) {
		write(os, instr);
		os << endl;
	}
}

void AttWriter::write(ostream& os, const OpSet& o) {
	os << "{ ";
	for ( uint64_t r = 0; r < 16; ++r )
		if ( o.contains((R64)r) ) { 
			write(os, (R64)r);
			os << " ";
		}
		else if ( o.contains((R32)r) ) {
			write(os, (R32)r);		
			os << " ";
		}
		else if ( o.contains((R16)r) ) {
			write(os, (R16)r);
			os << " ";
		}
		else if ( r < 4 && o.contains((Rh)(r+4)) ) {
			write(os, (Rh)(r+4));
			os << " ";
		}
		else if ( o.contains((Rl)r) ) {
			write(os, (Rl)r);
			os << " ";
		}
	os << "}" << endl;

	os << "{ ";
	for ( uint64_t x = 0; x < 16; ++x )
		if ( o.contains((Ymm)x) ) {
			write(os, (Ymm)x);
			os << " ";
		}
		else if ( o.contains((Xmm)x) ) {
			write(os, (Xmm)x);
			os << " ";
		}
	os << "}" << endl;

	os << "{ ";
	for ( uint64_t m = 0; m < 8; ++m )
		if ( o.contains((Mm)m) ) {
			write(os, (Mm)m);
			os << " ";
		}
	os << "}";
}

} // namespace x64
