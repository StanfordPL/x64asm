#include "src/cfg/control_flow_graph.h"

#include <map>

#include "src/code/label.h"
#include "src/att/att_writer.h"

using namespace std;

namespace x64 {

void ControlFlowGraph::recompute_blocks() {
	blocks_.clear();
	preds_.clear();
	succs_.clear();
	exits_.clear();

	// Record block begins
	blocks_.push_back(0);
	for ( size_t i = 1, ie = code_.size(); i < ie; ++i ) {
		const auto& instr = code_[i];

		// Labels or the first instruction define the beginning of a block
		if ( instr.is_label_defn() )
			blocks_.push_back(i);

		// Jumps, returns, and last instructions end blocks.
		// We increment i to avoid double counts for labels. 
		if ( (instr.is_jump() || instr.is_ret()) && (i+1 != ie) ) {
			blocks_.push_back(i+1);
			if ( code_[i+1].is_label_defn() )
				i++;
		}
	}
	blocks_.push_back(code_.size());
	num_blocks_ = blocks_.size()-1;

	// Record label -> block mapping
	map<Label, id_type> labels;
	for ( size_t i = 0, ie = num_blocks(); i < ie; ++i ) {
		const auto& instr = *instr_begin(i);
		if ( instr.is_label_defn() )
			labels[instr.get_operand(0)] = i;
	}

	// Set up successors
	succs_.resize(num_blocks());
	for ( size_t i = 0, ie = num_blocks(); i < ie; ++i ) {
		const auto& instr = *(instr_end(i)-1);
		
		// Unconditional jumps have non-trivial fallthrough targets.
		if ( instr.is_uncond_jump() ) {
			succs_[i].push_back(labels[instr.get_operand(0)]);
			continue;
		}

		// Returns and the final instruction, have no fallthrough targets.
		if ( instr.is_ret() || (i+1) == ie ) 
			continue;

		// Everything else has a fallthrough and a possible conditional target.
		succs_[i].push_back(i+1);			
		if ( instr.is_cond_jump() )
			succs_[i].push_back(labels[instr.get_operand(0)]);
	}

	// Set up predecessors (these have no particular order) and exits
	preds_.resize(num_blocks());
	for ( size_t i = 0, ie = num_blocks(); i < ie; ++i )
		if ( is_exit(i) )
			exits_.push_back(i);
		else
			for ( auto s = succ_begin(i), se = succ_end(i); s != se; ++s )
				preds_[*s].push_back(i);
}

void ControlFlowGraph::recompute_liveness() {
	// Compute gen/kill sets for blocks
	vector<RegSet> gen(num_blocks());
	vector<RegSet> kill(num_blocks());

	for ( size_t i = 0, ie = num_blocks(); i < ie; ++i )
		for ( int j = blocks_[i+1]-1, je = blocks_[i]; j >= je; --j ) {
			const auto& instr = code_[j];
			kill[i] |= instr.write_set();
			kill[i] |= instr.undef_set();
			gen[i]  |= instr.read_set();
		}
			
	vector<RegSet> live_ins(num_blocks());
	live_outs_.resize(num_blocks());

	// Boundary conditions
	const auto bound = 
		RegSet().set(rax).set(rbp).set(rsp);
	for ( size_t i = 0, ie = num_blocks(); i < ie; ++i )
		if ( is_exit(i) )
			live_outs_[i] = bound;

	// Iterate until fixed point
	for ( bool changed = true; changed; ) {
		changed = false;

		// Meet operator
		for ( size_t i = 0, ie = num_blocks(); i < ie; ++i )
			if ( !is_exit(i) ) {
				RegSet new_out;
				for ( auto s = succ_begin(i), se = succ_end(i); s != se; ++s )
					new_out |= live_ins[*s];

				changed |= (live_outs_[i] != new_out);
				live_outs_[i] = new_out;
			}

		// Transfer function
		for ( size_t i = 0, ie = num_blocks(); i < ie; ++i ) {
			RegSet new_in = live_outs_[i];
			new_in -= kill[i];
			new_in |= gen[i];

			changed |= (live_ins[i] != new_in);
			live_ins[i] = new_in;
		}
	}
}

void ControlFlowGraph::recompute_defs() {
	// Compute gen sets for blocks
	vector<RegSet> gen(num_blocks());
	for ( size_t i = 0, ie = num_blocks(); i < ie; ++i )
		for ( auto j = instr_begin(i), je = instr_end(i); j != je; ++j ) {
			gen[i] |= j->write_set();
			gen[i] -= j->undef_set();
		}

	def_ins_.resize(num_blocks());
	vector<RegSet> def_outs(num_blocks());

	// Boundary conditions
	for ( size_t i = 0, ie = num_blocks(); i < ie; ++i )
		if ( preds_[i].empty() )
			def_ins_[i] = get_inputs();

	// Iterate until fixed point
	for ( bool changed = true; changed; ) {
		changed = false;

		// Meet operator
		for ( size_t i = 0, ie = num_blocks(); i < ie; ++i )
			if ( !preds_[i].empty() ) {
				RegSet new_in = ~RegSet();
				for ( auto p = pred_begin(i), pe = pred_end(i); p != pe; ++p )
					new_in &= def_outs[*p];

				changed |= def_ins_[i] != new_in;
				def_ins_[i] = new_in;
			}

		// Transfer function
		for ( size_t i = 0, ie = num_blocks(); i < ie; ++i ) {
			RegSet new_out = def_ins_[i];
			new_out |= gen[i];

			changed |= def_outs[i] != new_out;
			def_outs[i] = new_out;
		}
	}
}

void ControlFlowGraph::write_dot(ostream& os) const {
	AttWriter writer;

	os << "digraph g {" << endl;

	os << "entry [shape=box label=\"ENTRY\"];" << endl;
	os << "exit  [shape=box label=\"EXIT\"];" << endl;

	for ( size_t i = 0, ie = num_blocks(); i < ie; ++i ) {
		os << "bb" << dec << i << "[shape=record label=\"{";

		os << "live-ins:";
		const auto lis = get_live_ins(location_type(i, 0));
		/*
		for ( int r = 0; r < 16; ++r ) {
			const auto w = lis.get_widest_set(R(r));
			if ( w != BIT_WIDTH_NULL ) {
				os << " ";
				writer.write(os, GpReg(r), w);	
			}
		}
		*/
		os << "|";

		for ( auto j = instr_begin(i), je = instr_end(i); j != je; ++j ) {
			writer.write(os, *j);
			os << "\\l";
		}

		os << "|live-outs:";
		const auto los = get_live_outs(location_type(i, num_instrs(i)-1));
		/*
		for ( auto r = 0; r < 16; ++r ) {
			const auto w = los.get_widest_set(GpReg(r));
			if ( w != BIT_WIDTH_NULL ) {
				os << " ";
				writer.write(os, GpReg(r), w);
			}
		}
		*/

		os << "}\"];" << endl;
	}

	for ( size_t i = 0, ie = num_blocks(); i < ie; ++i )
		for ( auto s = succ_begin(i), se = succ_end(i); s != se; ++s ) {
			os << "bb" << dec << i << "->bb" << *s << " [style="; 
			if ( has_fallthrough_target(i) && get_fallthrough_target(i) == *s )
				os << "bold";
			else
				os << "dashed";
			os << "];" << endl;
		}

	if ( num_blocks() > 0 )
		os << "entry->bb" << get_entry() << ";" << endl;
	for ( size_t i = 0, ie = num_blocks(); i < ie; ++i )
		if ( is_exit(i) )
			os << "bb" << i << "->exit;" << endl;

	os << "}";
}

} // namespace x64
