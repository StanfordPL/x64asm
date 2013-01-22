#include "src/cfg/defined_register.h"

using namespace std;

namespace x64 {

void DefinedRegister::recompute() {
	ins_.resize(cfg_.num_blocks(), OpSet::empty());
	outs_.resize(cfg_.num_blocks(), OpSet::empty());

	// Compute gen sets for blocks
	vector<OpSet> gen(cfg_.num_blocks(), OpSet::empty());
	for ( size_t i = cfg_.get_entry()+1, ie = cfg_.get_exit(); i <= ie; ++i )
		for ( auto j = cfg_.instr_begin(i), je = cfg_.instr_end(i); j != je; ++j ) {
			gen[i] |= j->must_write_set();
			gen[i] -= j->must_undef_set();
		}

	// Boundary conditions
	outs_[cfg_.get_entry()] = boundary_;

	// Initial conditions
	for ( size_t i = cfg_.get_entry()+1, ie = cfg_.get_exit(); i <= ie; ++i )
		outs_[i] = OpSet::universe();

	// Iterate until fixed point
	for ( bool changed = true; changed; ) {
		changed = false;

		// Meet operator
		for ( size_t i = cfg_.get_entry()+1, ie = cfg_.get_exit(); i <= ie; ++i ) {
			OpSet new_in = OpSet::universe();
			for ( auto p = cfg_.pred_begin(i), pe = cfg_.pred_end(i); p != pe; ++p )
				new_in &= outs_[*p];

			changed |= ins_[i] != new_in;
			ins_[i] = new_in;
		}

		// Transfer function
		for ( size_t i = cfg_.get_entry()+1, ie = cfg_.get_exit(); i <= ie; ++i ) {
			OpSet new_out = ins_[i];
			new_out |= gen[i];

			changed |= outs_[i] != new_out;
			outs_[i] = new_out;
		}
	}
}

} // namespace x64
