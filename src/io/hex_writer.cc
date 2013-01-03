#include "src/io/hex_writer.h"

#include <unordered_set>

#include "src/assembler/assembler.h"
#include "src/assembler/function.h"

using namespace std;

namespace x64 {

void HexWriter::write(ostream& os, const Code& c) {
	Assembler assm;
	Function fxn;
	unordered_set<size_t> eols;

	assm.start(fxn);
	for ( const auto& instr : c ) {
		assm.assemble(instr);
		eols.insert(fxn.size());
	}
	assm.finish();

	for ( size_t i = 0, ie = fxn.size(); i < ie; ++i ) {
		if ( eols.find(i) != eols.end() )
			os << endl;
		os << hex << noshowbase << fxn[i] << " ";
	}
	os << endl;
}

} // namespace x64

