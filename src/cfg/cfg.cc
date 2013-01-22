#include "src/cfg/cfg.h"

#include <unordered_map>

#include "src/code/label.h"

using namespace std;

namespace x64 {

void Cfg::recompute() {
	// Quick exit for corner case of empty code
	if ( code_.empty() ) {
		blocks_ = vector<size_t>{{ 0, 0, 0 }};
		preds_ = vector<vector<size_t>>{{ {{}}, {{0}} }};
		succs_ = vector<vector<size_t>>{{ {{1}}, {{}} }};
		return;
	}

	// Otherwise, clear everything and take the slow road
	blocks_.clear();
	preds_.clear();
	succs_.clear();

	// For keeping track of labels which begin blocks
	unordered_map<uint64_t, id_type> labels;

	// Push the ENTRY and first block indices (always zero)
	blocks_.push_back(0);
	blocks_.push_back(0);

	// Check whether the first instruction is a label, we skip it below.
	const auto& first = code_[0];
	if ( first.is_label_defn() )
		labels[((Label*)(first.get_operand(0)))->val()] = 1;

	// Iterate over the remaining instructions
	for ( size_t i = 1, ie = code_.size(); i < ie; ++i ) {
		const auto& instr = code_[i];

		// Labels begin blocks (watch out for double counting; see below)
		if ( instr.is_label_defn() ) {
			if ( blocks_.back() != i )
				blocks_.push_back(i);
			labels[((Label*)(instr.get_operand(0)))->val()] = blocks_.size()-1;
			continue;
		}
		// Jumps and returns end blocks (this can double count a label)
		if ( instr.is_jump() || instr.is_return() )
			blocks_.push_back(i+1);
	}

	// Push the EXIT and sentinel indices (we may already have caught the exit)
	if ( blocks_.back() != code_.size() )
		blocks_.push_back(code_.size());
	blocks_.push_back(code_.size());

	// Successors
	succs_.resize(num_blocks());
	for ( size_t i = get_entry(), ie = get_exit(); i < ie; ++i ) {
		// Empty blocks point forward (this handles ENTRY)
		if ( num_instrs(i) == 0 ) {
			succs_[i].push_back(i+1);
			continue;
		}

		// Grab the last instruction in the block
		const auto& instr = code_[blocks_[i+1]-1];

		// Unconditional jumps have non-trivial fallthrough targets.
		if ( instr.is_uncond_jump() ) {
			succs_[i].push_back(labels[((Label*)(instr.get_operand(0)))->val()]);
			continue;
		}
		// Returns point to exit
		if ( instr.is_return() ) {
			succs_[i].push_back(get_exit());
			continue;
		}
		// Everything else has a fallthrough and a possible conditional target.
		succs_[i].push_back(i+1);			
		if ( instr.is_cond_jump() )
			succs_[i].push_back(labels[((Label*)(instr.get_operand(0)))->val()]);
	}

	// Predecessors 
	preds_.resize(num_blocks());
	for ( size_t i = get_entry(), ie = get_exit(); i < ie; ++i )
		for ( auto s = succ_begin(i), se = succ_end(i); s != se; ++s )
			preds_[*s].push_back(i);
}

} // namespace x64
