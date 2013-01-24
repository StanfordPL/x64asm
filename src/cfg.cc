#include "src/cfg.h"

#include <algorithm>
#include <stack>
#include <unordered_map>

#include "src/label.h"

using namespace std;

namespace x64 {

void Cfg::recompute_blocks() {
	// Quick exit for corner case of empty code
	if ( code_.empty() ) {
		blocks_ = vector<size_t> { 0, 0, 0 };
		preds_ = vector<vector<size_t>> { {}, {0} };
		succs_ = vector<vector<size_t>> { {1}, {} };
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

void Cfg::recompute_defs(const OpSet& def_ins) {
	def_ins_.resize(num_blocks(), OpSet::empty());
	def_outs_.resize(num_blocks(), OpSet::empty());

	// Compute gen sets for blocks
	vector<OpSet> gen(num_blocks(), OpSet::empty());
	for ( size_t i = get_entry()+1, ie = get_exit(); i <= ie; ++i )
		for ( auto j = instr_begin(i), je = instr_end(i); j != je; ++j ) {
			gen[i] |= j->must_write_set();
			gen[i] -= j->must_undef_set();
		}

	// Boundary conditions
	def_outs_[get_entry()] = def_ins;

	// Initial conditions
	for ( size_t i = get_entry()+1, ie = get_exit(); i <= ie; ++i )
		def_outs_[i] = OpSet::universe();

	// Iterate until fixed point
	for ( bool changed = true; changed; ) {
		changed = false;

		// Meet operator
		for ( size_t i = get_entry()+1, ie = get_exit(); i <= ie; ++i ) {
			OpSet new_in = OpSet::universe();
			for ( auto p = pred_begin(i), pe = pred_end(i); p != pe; ++p )
				new_in &= def_outs_[*p];

			changed |= def_ins_[i] != new_in;
			def_ins_[i] = new_in;
		}

		// Transfer function
		for ( size_t i = get_entry()+1, ie = get_exit(); i <= ie; ++i ) {
			OpSet new_out = def_ins_[i];
			new_out |= gen[i];

			changed |= def_outs_[i] != new_out;
			def_outs_[i] = new_out;
		}
	}
}

void Cfg::recompute_dominators() {
	dom_ins_.resize(num_blocks());
	dom_outs_.resize(num_blocks());

	// Bounary conditions
	dom_outs_[get_entry()].clear();
	dom_outs_[get_entry()].insert(get_entry());

	// Initial conditions
	unordered_set<id_type> top;
	for ( size_t i = get_entry(), ie = get_exit(); i < ie; ++i )
		top.insert(i);
	for ( size_t i = get_entry()+1, ie = get_exit(); i <= ie; ++i )
		dom_outs_[i] = top;

	// Iterate until fixed point 
	for ( bool changed = true; changed; ) {
		changed = false;

		// Meet operator
		for ( size_t i = get_entry()+1, ie = get_exit(); i <= ie; ++i ) {
			auto new_in = top;
			for ( auto p = pred_begin(i), pe = pred_end(i); p != pe; ++p )
				new_in.erase(dom_outs_[*p].begin(), dom_outs_[*p].end());

			changed |= dom_ins_[i] != new_in;
			dom_ins_[i] = new_in;
		}

		// Transfer function
		for ( size_t i = get_entry()+1, ie = get_exit(); i <= ie; ++i ) {
			auto new_out = dom_ins_[i];
			new_out.insert(i);

			changed |= dom_outs_[i] != new_out;
			dom_outs_[i] = new_out;
		}		
	}
}

void Cfg::recompute_liveness(const OpSet& live_outs) {
	live_ins_.resize(num_blocks(), OpSet::empty());
	live_outs_.resize(num_blocks(), OpSet::empty());

	// Compute gen/kill sets for blocks
	vector<OpSet> gen(num_blocks(), OpSet::empty());
	vector<OpSet> kill(num_blocks(), OpSet::empty());

	for ( size_t i = get_entry(), ie = get_exit(); i < ie; ++i )
		for ( auto j = instr_rbegin(i), je = instr_rend(i); j != je; ++j ) {
			const auto use = j->must_read_set();
			const auto def = j->must_write_set() | j->must_undef_set();

			kill[i] |= def;
			kill[i] -= use;

			gen[i]  -= def;
			gen[i]  |= use;
		}
		
	// Boundary conditions
	live_ins_[get_exit()] = live_outs;

	// Initial conditions
	for ( size_t i = get_entry(), ie = get_exit(); i < ie; ++i )
		live_ins_[i] = OpSet::empty();

	// Iterate until fixed point
	for ( bool changed = true; changed; ) {
		changed = false;

		// Meet operator
		for ( size_t i = get_entry(), ie = get_exit(); i < ie; ++i ) {
			OpSet new_out = OpSet::empty();
			for ( auto s = succ_begin(i), se = succ_end(i); s != se; ++s )
				new_out |= live_ins_[*s];

			changed |= (live_outs_[i] != new_out);
			live_outs_[i] = new_out;
		}

		// Transfer function
		for ( size_t i = get_entry(), ie = get_exit(); i < ie; ++i ) {
			OpSet new_in = live_outs_[i];
			new_in -= kill[i];
			new_in |= gen[i];

			changed |= (live_ins_[i] != new_in);
			live_ins_[i] = new_in;
		}
	}
}

void Cfg::recompute_loops() {
	back_edges_.clear();
	for ( auto i = reachable_.begin(), ie = reachable_.end(); i != ie; ++i )
		for ( auto s = succ_begin(*i), se = succ_end(*i); s != se; ++s )
			if ( dom(*s, *i) )
				back_edges_.push_back(make_pair(*i, *s));

	nesting_depth_.resize(num_blocks());
	for ( auto& d : nesting_depth_ )
		d = 0;

	loops_.clear();
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

void Cfg::recompute_reachable() {
	reachable_.clear();

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

void Cfg::write_att(ostream& os) const {
	write_txt(os, true);
}

void Cfg::write_intel(ostream& os) const {
	write_txt(os, false);
}

void Cfg::write_txt(ostream& os, bool att) const {
	os << "digraph g {" << endl;

	os << "colorscheme = blues6" << endl;

	os << "bb" << get_entry() << " [shape=box label=\"ENTRY\"];" << endl;
	os << "bb" << get_exit()  << " [shape=box label=\"EXIT\"];" << endl;

	map<size_t, vector<Cfg::id_type>> nestings;
	for ( size_t i = get_entry()+1, ie = get_exit(); i < ie; ++i )
		nestings[nesting_depth(i)].push_back(i);

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
			os << "|live-outs:";
			os << "|";
			for ( size_t j = 0, je = num_instrs(bb); j < je; ++j ) {
				const auto& instr = get_instr(location_type(bb,j));
				if ( att )
					instr.write_att(os);
				else
					instr.write_intel(os);
				os << "\\l";
			}
			os << "}\"];" << endl;
		}
	}
	for ( size_t i = 0, ie = nestings.size(); i < ie; ++i )
		os << "}" << endl;

	for ( size_t i = get_entry(), ie = get_exit(); i < ie; ++i ) 
		for ( auto s = succ_begin(i), se = succ_end(i); s != se; ++s ) {
			os << "bb" << dec << i << "->bb" << *s << " [";
			os << "style="; 
			if ( has_fallthrough_target(i) && 
					 get_fallthrough_target(i) == *s )
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
