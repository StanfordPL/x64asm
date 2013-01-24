#ifndef X64_SRC_CODE_CFG_H
#define X64_SRC_CODE_CFG_H

#include <cassert>
#include <iostream>
#include <map>
#include <unordered_set>
#include <vector>

#include "src/code/code.h"
#include "src/code/instruction.h"
#include "src/code/op_set.h"

namespace x64 {

/** A Control Flow Graph. */
class Cfg {
	public:
		typedef size_t id_type;
		typedef std::pair<id_type, size_t> location_type;

		Cfg(const Code& code) 
				: code_{code} { 
			recompute(OpSet::empty(), OpSet::empty());
		}

		Cfg(const Code& code, const OpSet& def_ins, const OpSet& live_outs) 
				: code_{code} {
			recompute(def_ins, live_outs);
		}

		inline void recompute(const OpSet& def_ins, const OpSet& live_outs) {
			recompute_blocks();
			recompute_dominators();
			recompute_loops();
			recompute_reachable();
			recompute_defs(def_ins);
			recompute_liveness(live_outs);
		}

		void recompute_blocks();

		void recompute_defs(const OpSet& def_in);

    void recompute_dominators();

		void recompute_liveness(const OpSet& live_out);

		void recompute_loops();

		void recompute_reachable();

		inline const Code& get_code() const {
			return code_;
		}

		inline size_t num_blocks() const {
			return blocks_.size() - 1;
		}

		inline size_t num_instrs(id_type id) const {
			assert(id < num_blocks());
			return blocks_[id+1] - blocks_[id];
		}

		inline id_type get_entry() const {
			return 0;
		}

		inline bool is_entry(id_type id) const {
			return id == get_entry();
		}

		inline id_type get_exit() const {
			return num_blocks()-1;
		}

		inline bool is_exit(id_type id) const {
			return id == get_exit();
		}

		inline const Instruction& get_instr(const location_type& loc) const {
			assert(get_index(loc) < code_.size());
			return code_[get_index(loc)];
		}

		typedef Code::const_iterator instr_iterator;

		inline instr_iterator instr_begin(id_type id) const {
			assert(id < num_blocks());
			return code_.begin() + blocks_[id];
		}

		inline instr_iterator instr_end(id_type id) const {
			assert(id < num_blocks());
			return code_.begin() + blocks_[id+1];
		}	

		typedef Code::const_reverse_iterator instr_reverse_iterator;

		inline instr_reverse_iterator instr_rbegin(id_type id) const {
			return instr_reverse_iterator(instr_end(id));
		}

		inline instr_reverse_iterator instr_rend(id_type id) const {
			return instr_reverse_iterator(instr_begin(id));;
		}	

		typedef std::vector<id_type>::const_iterator pred_iterator;

		inline pred_iterator pred_begin(id_type id) const {
			assert(id < num_blocks());
			return preds_[id].begin();
		}

		inline pred_iterator pred_end(id_type id) const {
			assert(id < num_blocks());
			return preds_[id].end();
		}

		typedef std::vector<id_type>::const_iterator succ_iterator;

		inline succ_iterator succ_begin(id_type id) const {
			assert(id < num_blocks());
			return succs_[id].begin();
		}

		inline succ_iterator succ_end(id_type id) const {
			assert(id < num_blocks());
			return succs_[id].end();
		}

		inline bool has_fallthrough_target(id_type id) const {
			assert(id < num_blocks());
			return !succs_[id].empty();
		}

		inline id_type get_fallthrough_target(id_type id) const {
			assert(has_fallthrough_target(id));
			return succs_[id][0];
		}

		inline bool has_conditional_target(id_type id) const {
			assert(id < num_blocks());
			return succs_[id].size() == 2;
		}

		inline id_type get_conditional_target(id_type id) const {
			assert(has_conditional_target(id));
			return succs_[id][1];
		}

		// Definition queries

		inline OpSet def_ins(id_type id) const {
			assert(id < num_blocks());
			assert(!is_entry(id));
			return def_ins_[id];
		}

		inline OpSet def_ins(const location_type& loc) const {
			auto os = def_ins(loc.first);
			// TODO...
			return os;
		}

		inline OpSet def_outs(id_type id) const {
			assert(id < num_blocks());
			return def_outs_[id];
		}

		inline OpSet def_outs(const location_type& loc) const {
			auto os = def_ins(loc.first);
			// TODO...
			return os;
		}

		// Dominator queries

		typedef std::unordered_set<id_type>::const_iterator dom_iterator;

		inline dom_iterator dom_in_begin(id_type id) const {
			assert(id < num_blocks());
		 	assert(!is_entry(id));
			return dom_ins_[id].begin();
		}

		inline dom_iterator dom_in_end(id_type id) const {
			assert(id < num_blocks());
		 	assert(!is_entry(id));
			return dom_ins_[id].end();
		}

		inline dom_iterator dom_out_begin(id_type id) const {
			assert(id < num_blocks());
			return dom_outs_[id].begin();
		}

		inline dom_iterator dom_out_end(id_type id) const {
			assert(id < num_blocks());
			return dom_outs_[id].end();
		}

		inline bool dom(id_type x, id_type y) const {
			assert(x < num_blocks());
			assert(y < num_blocks());
			const auto& dominators = dom_outs_[y];
			return dominators.find(x) != dominators.end();
		}

		// Liveness queries

		inline OpSet live_ins(id_type id) const {
			assert(id < num_blocks());
			return live_ins_[id];
		}

		inline OpSet live_ins(const location_type& loc) const {
			auto os = live_outs(loc.first);
			// TODO...
			return os;
		}

		inline OpSet live_outs(id_type id) const {
			assert(id < num_blocks());
			assert(!is_exit(id));
			return live_outs_[id];
		}

		inline OpSet live_outs(const location_type& loc) const {
			auto os = live_outs(loc.first);
			// TODO...
			return os;
		}

		// Loop queries

		typedef std::pair<id_type, id_type> edge_type;
		typedef std::unordered_set<id_type> loop_type;

		typedef std::vector<edge_type>::const_iterator back_edge_iterator;

		inline back_edge_iterator back_edge_begin() const {
			return back_edges_.begin();
		}

		inline back_edge_iterator back_edge_end() const {
			return back_edges_.end();
		}

		typedef loop_type::const_iterator loop_iterator;

		inline loop_iterator loop_begin(const edge_type& be) const {
			const auto itr = loops_.find(be);
			assert(itr != loops_.end());
			return itr->second.begin();
		}

		inline loop_iterator loop_end(const edge_type& be) const {
			const auto itr = loops_.find(be);
			assert(itr != loops_.end());
			return itr->second.end();
		}

		inline size_t nesting_depth(id_type id) const {
			assert(id < num_blocks());
			return nesting_depth_[id];
		}

		// Reachable queries

		typedef std::unordered_set<Cfg::id_type>::const_iterator reachable_iterator;

		inline reachable_iterator begin() const {
			return reachable_.begin();
		}

		inline reachable_iterator end() const {
			return reachable_.end();
		}

		inline bool is_reachable(id_type id) const {
			assert(id < num_blocks());
			assert(!is_entry(id));
			assert(!is_exit(id));
			return reachable_.find(id) != reachable_.end();
		}

		void write_att(std::ostream& os) const;
		void write_intel(std::ostream& os) const;

	private:
		const Code& code_;

		// [ENTRY][b0 begin][b0 end/b1 begin]...[bn end/EXIT][SENTINEL]
		std::vector<size_t> blocks_;
		std::vector<std::vector<id_type>> preds_;
		std::vector<std::vector<id_type>> succs_;

		// Definitions
		std::vector<OpSet> def_ins_;
		std::vector<OpSet> def_outs_;

		// Dominators
		std::vector<std::unordered_set<Cfg::id_type>> dom_ins_;
		std::vector<std::unordered_set<Cfg::id_type>> dom_outs_;

		// Liveness
		std::vector<OpSet> live_ins_;
		std::vector<OpSet> live_outs_;

		// Loops
		std::vector<edge_type> back_edges_;
		std::map<edge_type, loop_type> loops_;
		std::vector<size_t> nesting_depth_;

		// Reachable blocks (does not include ENTRY or EXIT)
		std::unordered_set<id_type> reachable_;

		inline size_t get_index(const location_type& loc) const {
			assert(loc.first < num_blocks());
			assert(loc.second < num_instrs(loc.first));
			return blocks_[loc.first] + loc.second;
		}

		void write_txt(std::ostream& os, bool att) const;
};

} // namespace x64
#endif
