#include "src/cfg/live_register.h"

using namespace std;

namespace x64 {

void LiveRegister::recompute() {
	ins_.resize(cfg_.num_blocks(), OpSet::empty());
	outs_.resize(cfg_.num_blocks(), OpSet::empty());

	// Compute gen/kill sets for blocks
	vector<OpSet> gen(cfg_.num_blocks(), OpSet::empty());
	vector<OpSet> kill(cfg_.num_blocks(), OpSet::empty());

	for ( size_t i = cfg_.get_entry(), ie = cfg_.get_exit(); i < ie; ++i )
		for ( auto j = cfg_.instr_rbegin(i), je = cfg_.instr_rend(i); j != je; ++j ) {
			const auto use = j->must_read_set();
			const auto def = j->must_write_set() | j->must_undef_set();

			kill[i] |= def;
			kill[i] -= use;

			gen[i]  -= def;
			gen[i]  |= use;
		}
		
	// Boundary conditions
	ins_[cfg_.get_exit()] = boundary_;

	// Initial conditions
	for ( size_t i = cfg_.get_entry(), ie = cfg_.get_exit(); i < ie; ++i )
		ins_[i] = OpSet::empty();

	// Iterate until fixed point
	for ( bool changed = true; changed; ) {
		changed = false;

		// Meet operator
		for ( size_t i = cfg_.get_entry(), ie = cfg_.get_exit(); i < ie; ++i ) {
			OpSet new_out = OpSet::empty();
			for ( auto s = cfg_.succ_begin(i), se = cfg_.succ_end(i); s != se; ++s )
				new_out |= ins_[*s];

			changed |= (outs_[i] != new_out);
			outs_[i] = new_out;
		}

		// Transfer function
		for ( size_t i = cfg_.get_entry(), ie = cfg_.get_exit(); i < ie; ++i ) {
			OpSet new_in = outs_[i];
			new_in -= kill[i];
			new_in |= gen[i];

			changed |= (ins_[i] != new_in);
			ins_[i] = new_in;
		}
	}
}

} // namespace x64
