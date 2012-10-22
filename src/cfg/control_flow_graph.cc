#include "src/cfg/control_flow_graph.h"

#include <algorithm>
#include <map>
#include <stack>

#include "src/code/label.h"
#include "src/att/att_writer.h"

using namespace std;
using namespace x64;

namespace {

void write(ostream& os, const AttWriter& w, const RegSet& rs) {
	for ( auto i = R64::begin(), ie = R64::end(); i != ie; ++i ) {
		if ( rs.is_set((R64)*i) )
			w.write(os, (R64)*i);
		else if ( rs.is_set((R32)*i) )
			w.write(os, (R32)*i);
		else if ( rs.is_set((R16)*i) )
			w.write(os, (R16)*i);
		else if ( rs.is_set((R8)*i) )
			w.write(os, (R8)*i);
		else if ( *i <= bh && rs.is_set((RH)*i) )
			w.write(os, (RH)*i);
	}
}

} // namespace

namespace x64 {

void ControlFlowGraph::recompute_blocks() {
	blocks_.clear();
	reachable_.clear();
	preds_.clear();
	succs_.clear();
	exits_.clear();

	// ENTRY Block
	blocks_.push_back(0);

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

	// EXIT / Sentinel blocks
	blocks_.push_back(code_.size());
	blocks_.push_back(code_.size());

	// Don't count the sentinel block
	num_blocks_ = blocks_.size()-1;

	// Record label -> block mapping
	map<Label, id_type> labels;
	for ( size_t i = get_entry(), ie = get_exit(); i < ie; ++i ) {
		// Don't bother checking empty blocks
		if ( num_instrs(i) == 0 )
			continue;

		const auto& instr = *instr_begin(i);
		if ( instr.is_label_defn() )
			labels[instr.get_operand(0)] = i;
	}

	// Set up successors
	succs_.resize(num_blocks());
	for ( size_t i = get_entry(), ie = get_exit(); i < ie; ++i ) {
		// Empty blocks just point forward (this handles entry)
		if ( num_instrs(i) == 0 ) {
			succs_[i].push_back(i+1);
			continue;
		}

		const auto& instr = *(instr_end(i)-1);
		
		// Unconditional jumps have non-trivial fallthrough targets.
		if ( instr.is_uncond_jump() ) {
			succs_[i].push_back(labels[instr.get_operand(0)]);
			continue;
		}

		// Returns point to exit
		if ( instr.is_ret() ) {
			succs_[i].push_back(get_exit());
			continue;
		}

		// Everything else has a fallthrough and a possible conditional target.
		succs_[i].push_back(i+1);			
		if ( instr.is_cond_jump() )
			succs_[i].push_back(labels[instr.get_operand(0)]);
	}

	// Set up predecessors (these have no particular order) and exits
	preds_.resize(num_blocks());
	for ( size_t i = get_entry(), ie = get_exit(); i < ie; ++i )
		for ( auto s = succ_begin(i), se = succ_end(i); s != se; ++s )
			preds_[*s].push_back(i);

	// Set up reachable blocks
	stack<id_type> r;
	r.push(get_entry());

	while ( !r.empty() ) {
		const auto m = r.top();
		r.pop();
		for ( auto s = succ_begin(m), se = succ_end(m); s != se; ++s )
			if ( !is_exit(*s) && reachable_.insert(*s).second )
				r.push(*s);
	}
}

void ControlFlowGraph::recompute_liveness() {
	live_ins_.resize(num_blocks());
	live_outs_.resize(num_blocks());

	// Compute gen/kill sets for blocks
	vector<RegSet> gen(num_blocks());
	vector<RegSet> kill(num_blocks());

	for ( size_t i = get_entry()+1, ie = get_exit(); i < ie; ++i )
		for ( int j = blocks_[i+1]-1, je = blocks_[i]; j >= je; --j ) {
			const auto& instr = code_[j];
			const auto use = instr.read_set();
			const auto def = instr.write_set() | instr.undef_set();

			kill[i] |= def;
			kill[i] -= use;

			gen[i]  -= def;
			gen[i]  |= use;
		}
		
	// Boundary / Initial conditions
	live_ins_[get_exit()] = RegSet().set(rbp).set(rsp).set(rax);
	for ( size_t i = get_entry()+1, ie = get_exit(); i < ie; ++i )
		live_ins_[i] = RegSet();

	// Iterate until fixed point
	for ( bool changed = true; changed; ) {
		changed = false;

		// Meet operator
		for ( size_t i = get_entry()+1, ie = get_exit(); i < ie; ++i ) {
			RegSet new_out;
			for ( auto s = succ_begin(i), se = succ_end(i); s != se; ++s )
				new_out |= live_ins_[*s];

			changed |= (live_outs_[i] != new_out);
			live_outs_[i] = new_out;
		}

		// Transfer function
		for ( size_t i = get_entry()+1, ie = get_exit(); i < ie; ++i ) {
			RegSet new_in = live_outs_[i];
			new_in -= kill[i];
			new_in |= gen[i];

			changed |= (live_ins_[i] != new_in);
			live_ins_[i] = new_in;
		}
	}
}

void ControlFlowGraph::recompute_defs() {
	def_ins_.resize(num_blocks());
	def_outs_.resize(num_blocks());

	// Compute gen sets for blocks
	vector<RegSet> gen(num_blocks());
	for ( size_t i = get_entry()+1, ie = get_exit(); i < ie; ++i )
		for ( auto j = instr_begin(i), je = instr_end(i); j != je; ++j ) {
			gen[i] |= j->write_set();
			gen[i] -= j->undef_set();
		}

	// Boundary / initial conditions
	def_outs_[get_entry()] = get_inputs();
	for ( size_t i = get_entry()+1, ie = get_exit(); i < ie; ++i )
		def_outs_[i] = ~RegSet();

	// Iterate until fixed point
	for ( bool changed = true; changed; ) {
		changed = false;

		// Meet operator
		for ( size_t i = get_entry()+1, ie = get_exit(); i < ie; ++i ) {
			RegSet new_in = ~RegSet();
			for ( auto p = pred_begin(i), pe = pred_end(i); p != pe; ++p )
				new_in &= def_outs_[*p];

			changed |= def_ins_[i] != new_in;
			def_ins_[i] = new_in;
		}

		// Transfer function
		for ( size_t i = get_entry()+1, ie = get_exit(); i < ie; ++i ) {
			RegSet new_out = def_ins_[i];
			new_out |= gen[i];

			changed |= def_outs_[i] != new_out;
			def_outs_[i] = new_out;
		}
	}
}

void ControlFlowGraph::recompute_dominators() {
	assert(num_blocks() <= 64);

	dom_ins_.resize(num_blocks());
	dom_outs_.resize(num_blocks());

	// Bounary / initial conditions
	dom_outs_[get_entry()].reset().set(get_entry());
	for ( size_t i = get_entry()+1, ie = get_exit(); i < ie; ++i )
		dom_outs_[i].set();	

	// Iterate until fixed point 
	for ( bool changed = true; changed; ) {
		changed = false;

		// Meet operator
		for ( size_t i = get_entry()+1, ie = get_exit(); i < ie; ++i ) {
			bitset<64> new_in;
			new_in.set();
			for ( auto p = pred_begin(i), pe = pred_end(i); p != pe; ++p )
				new_in &= dom_outs_[*p];

			changed |= dom_ins_[i] != new_in;
			dom_ins_[i] = new_in;
		}

		// Transfer function
		for ( size_t i = get_entry()+1, ie = get_exit(); i < ie; ++i ) {
			auto new_out = dom_ins_[i];
			new_out.set(i);

			changed |= dom_outs_[i] != new_out;
			dom_outs_[i] = new_out;
		}		
	}
}

void ControlFlowGraph::recompute_back_edges() {
	back_edges_.clear();
	for ( auto i = reachable_begin(), ie = reachable_end(); i != ie; ++i )
		for ( auto s = succ_begin(*i), se = succ_end(*i); s != se; ++s )
			if ( dom_outs_[*i].test(*s) )
				back_edges_.push_back(make_pair(*i, *s));
}

void ControlFlowGraph::recompute_loops() {
	loops_.clear();
	nesting_depth_.resize(num_blocks());
	for ( size_t i = 0, ie = num_blocks(); i < ie; ++i )
		nesting_depth_[i] = 0;

	for ( const auto& e : back_edges_ ) {
		if ( e.first == e.second ) {
			loops_[e].insert(e.first);
			nesting_depth_[e.first]++;
			continue;
		}

		loop_type& l = loops_[e];
		l.insert(e.second);
		l.insert(e.first);

		stack<id_type> s;
		s.push(e.first);

		while ( !s.empty() ) {
			const auto m = s.top();
			s.pop();

			for ( auto p = pred_begin(m), pe = pred_end(m); p != pe; ++p )
				if ( is_reachable(*p) && *p != e.second && l.insert(*p).second )
					s.push(*p);
		}

		for ( const auto bb : l )
			nesting_depth_[bb]++;
	}
}

void ControlFlowGraph::write_dot(ostream& os) const {
	AttWriter writer;

	os << "digraph g {" << endl;

	os << "colorscheme = blues6" << endl;

	os << "bb" << get_entry() << " [shape=box label=\"ENTRY\"];" << endl;
	os << "bb" << get_exit()  << " [shape=box label=\"EXIT\"];" << endl;

	map<size_t, vector<id_type>> nestings;
	for ( size_t i = get_entry()+1, ie = get_exit(); i < ie; ++i )
		nestings[get_nesting_depth(i)].push_back(i);

	for ( const auto& n : nestings ) {
		os << dec;
		os << "subgraph cluster_" << n.first << " {" << endl;
		os << "style = filled" << endl;
		os << "color = " << (n.first+1) << endl;

		for ( const auto bb : n.second ) {
			os << "bb" << dec << bb << "[";
			os << "shape=record, style=filled, fillcolor=white, ";
			if ( !is_reachable(bb) )
				os << "color = grey, ";
			os << "label=\"{";
			os << "#" << bb; 
			os << "|live-ins:";
			const auto lis = get_live_ins(location_type(bb, 0));
			write(os, writer, lis);
			os << "|";
			for ( size_t j = 0, je = num_instrs(bb); j < je; ++j ) {
				const auto& instr = get_instr(location_type(bb,j));
				//os << "LIVE IN:";
				//write(os, writer, get_live_ins(location_type(bb,j)));
				//os << "\\l";
				writer.write(os, instr);
				os << "\\l";
			}
			os << "|live-outs:";
			const auto los = get_live_outs(location_type(bb, num_instrs(bb)-1));
			write(os, writer, los);
			os << "}\"];" << endl;
		}
	}
	for ( size_t i = 0, ie = nestings.size(); i < ie; ++i )
		os << "}" << endl;

	for ( size_t i = get_entry(), ie = get_exit(); i < ie; ++i ) 
		for ( auto s = succ_begin(i), se = succ_end(i); s != se; ++s ) {
			os << "bb" << dec << i << "->bb" << *s << " [";
			os << "style="; 
			if ( has_fallthrough_target(i) && get_fallthrough_target(i) == *s )
				os << "bold";
			else
				os << "dashed";
			os << " color=";
			if ( find(back_edge_begin(), back_edge_end(), edge_type(i,*s)) != 
					back_edge_end() )
				os << "red";
			else if ( is_reachable(i) || is_entry(i) )
				os << "black";
			else
				os << "grey";
			os << "];" << endl;
		}

	os << "}";
}

} // namespace x64
