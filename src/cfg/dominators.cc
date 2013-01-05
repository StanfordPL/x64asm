#include "src/cfg/dominators.h"

using namespace std;

namespace x64 {

void Dominators::recompute() {
	ins_.resize(cfg_.num_blocks());
	outs_.resize(cfg_.num_blocks());

	// Bounary conditions
	outs_[cfg_.get_entry()].clear();
	outs_[cfg_.get_entry()].insert(cfg_.get_entry());

	// Initial conditions
	unordered_set<Cfg::id_type> top;
	for ( size_t i = cfg_.get_entry(), ie = cfg_.get_exit(); i < ie; ++i )
		top.insert(i);
	for ( size_t i = cfg_.get_entry()+1, ie = cfg_.get_exit(); i <= ie; ++i )
		outs_[i] = top;

	// Iterate until fixed point 
	for ( bool changed = true; changed; ) {
		changed = false;

		// Meet operator
		for ( size_t i = cfg_.get_entry()+1, ie = cfg_.get_exit(); i <= ie; ++i ) {
			auto new_in = top;
			for ( auto p = cfg_.pred_begin(i), pe = cfg_.pred_end(i); p != pe; ++p )
				new_in.erase(outs_[*p].begin(), outs_[*p].end());

			changed |= ins_[i] != new_in;
			ins_[i] = new_in;
		}

		// Transfer function
		for ( size_t i = cfg_.get_entry()+1, ie = cfg_.get_exit(); i <= ie; ++i ) {
			auto new_out = ins_[i];
			new_out.insert(i);

			changed |= outs_[i] != new_out;
			outs_[i] = new_out;
		}		
	}
}

} // namespace x64
