#ifndef X64_SRC_CFG_CONTROL_FLOW_GRAPH_H
#define X64_SRC_CFG_CONTROL_FLOW_GRAPH_H

#include <bitset>
#include <cassert>
#include <iostream>
#include <map>
#include <set>
#include <vector>

#include "src/code/code.h"
#include "src/code/instruction.h"
#include "src/code/reg_set.h"

namespace x64 {

/** A read-only data structure that presents the ControlFlowGraph 
	  represented by Code.
*/
class ControlFlowGraph {
	public:
		typedef size_t id_type;
		typedef std::pair<id_type, size_t> location_type;
		typedef std::pair<id_type, id_type> edge_type;
		typedef std::set<id_type> loop_type;

		/** Creates a ControlFlowGraph.
			  This constructor will use liveness to determine inputs.
			  Use this only when your code has come from a reliable source.
			  @param code The Code that induces the graph.
		*/
		inline explicit ControlFlowGraph(const Code& code) 
				: code_(code) {
			recompute_blocks();
			recompute_liveness();		
			inputs_ = get_live_ins(location_type(1, 0));
			recompute_defs();
			recompute_dominators();
			recompute_back_edges();
			recompute_loops();
		}

		/** Creates a ControlFlowGraph.
			  Use this when your code has come from an unreliable source.
			  @param code The Code that induces the graph.
			  @param inputs Inputs to the ControlFlowGraph.
		*/
		inline explicit ControlFlowGraph(const Code& code, const RegSet& inputs)
				: code_(code), inputs_(inputs) {
			recompute();
		}

		/** Returns the underlying Code represented by the control flow graph.
		*/
		inline const Code& get_code() const {
			return code_;
		}

		/** Recomputes the entire control flow graph structure.
			  Computes live inputs using get_inputs().
		*/
		inline void recompute() {
			recompute_blocks();
			recompute_liveness();
			recompute_defs();
			recompute_dominators();
			recompute_back_edges();
			recompute_loops();
		}

		/** Recomputes the basic block structure of the control flow graph.
		*/
		void recompute_blocks();

		/** Reomputes liveness for the control flow graph.
		*/
		void recompute_liveness();

		/** Recomputes the registers defined at every program point.
		*/
		void recompute_defs();

		/** Recomputes dominators.
		*/
		void recompute_dominators();

		/** Recomputes back edges.
		*/
		void recompute_back_edges();

		/** Recomputes loops.
		*/
		void recompute_loops();

		/** Returns the inputs to the control flow graph.
		*/
		inline const RegSet& get_inputs() const {
			return inputs_;
		}	

		/** Returns the entry point to the control flow graph.
		*/
		inline id_type get_entry() const {
			return 0;
		}

		/** Returns true for the entry block
		*/
		inline bool is_entry(id_type id) const {
			return id == get_entry();
		}

		/** Returns the exit point from the control flow graph.
		*/
		inline id_type get_exit() const {
			return num_blocks()-1;
		}

		/** Returns true for the exit block
		*/
		inline bool is_exit(id_type id) const {
			return id == get_exit();
		}

		/** Returns the number of basic blocks in the control flow graph.
		*/
		inline size_t num_blocks() const { 
			return num_blocks_;
		}

		/** Returns the number of instructions in a basic block.
			  @param id Block id, asserts if out of range.
		*/
		inline size_t num_instrs(id_type id) const {
			assert(id < num_blocks());
			return blocks_[id+1] - blocks_[id];
		}

		/** Returns an instruction's location in the ControlFlowGraph.
		*/
		inline location_type get_location(size_t idx) const {
			// This will point you beyond empty blocks.  Great!
			assert(idx < code_.size());
			for ( int i = num_blocks()-1; i >= 0; --i )
				if ( idx >= blocks_[i] )
					return std::make_pair(i, idx - blocks_[i]);

			assert(false);
			return std::make_pair(0,0);
		}

		/** Maps a location back to a code index 
		*/
		inline size_t get_index(const location_type& loc) const {
			// This should be fine with empty blocks
			assert(loc.first < num_blocks());
			assert(loc.second < num_instrs(loc.first));
			return blocks_[loc.first] + loc.second;
		}

		/** Returns an instruction at a location in the control flow graph.
			  @param loc Location in the ControlFlowGraph, asserts if out of range.
		*/
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

		/** Returns true if a basic block has a fallthrough target.
			  @param id Block id, asserts if out of range.
		*/
		inline bool has_fallthrough_target(id_type id) const {
			assert(id < num_blocks());
			return !succs_[id].empty();
		}

		/** Returns the fallthrough target for a basic block.
			  @param id Block id, asserts if has_fallthrough_target(id) == false.
		*/
		inline id_type get_fallthrough_target(id_type id) const {
			assert(has_fallthrough_target(id));
			return succs_[id][0];
		}

		/** Returns true if a basic block ends in a conditional jump.
			  @param id Block id, asserts if out of range.
		*/
		inline bool has_conditional_target(id_type id) const {
			assert(id < num_blocks());
			return succs_[id].size() == 2;
		}

		/** Returns the conditional jump target for a basic block.
			  @param id Block id, asserts if has_conditional_target() == false.
		*/
		inline id_type get_conditional_target(id_type id) const {
			assert(has_conditional_target(id));
			return succs_[id][1];
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

		inline size_t get_nesting_depth(id_type id) const {
			assert(id < num_blocks());
			return nesting_depth_[id];
		}

		/** Returns the set of registers live in to an instruction.
			  @param loc ControlFlowGraph location, asserts if out of range.
		*/
		inline RegSet get_live_ins(const location_type& loc) const {
			auto rs = get_live_outs(loc);
			const auto& instr = code_[get_index(loc)];
			rs -= instr.write_set();
			rs -= instr.undef_set();
			rs |= instr.read_set();

			return rs;
		}

		/** Returns the set of registers live at a code point.
			  @param idx Instruction index in underlying code, asserts if out of range.
		*/
		inline RegSet get_live_ins(size_t idx) const {
			return get_live_ins(get_location(idx));
		}

		/** Returns the set of registers live out of an instruction.
			  @param loc ControlFlowGraph location, asserts if out of range.
		*/
		inline RegSet get_live_outs(const location_type& loc) const {
			auto rs = live_outs_[loc.first];
			for ( int i = blocks_[loc.first+1]-1, ie = blocks_[loc.first]+loc.second; i > ie; --i ) {
				const auto& instr = code_[i];
				rs -= instr.write_set();
				rs -= instr.undef_set();
				rs |= instr.read_set();
			}
			return rs;
		}

		/** Returns the set of registers live out at a code point.
			  @param idx Instruction index in underlying code, asserts if out of range.
		*/
		inline RegSet get_live_outs(size_t idx) const {
			return get_live_outs(get_location(idx));
		}

		/** Returns the set of registers definitely defined on entry to an 
			  instruction.
			  @param loc ControlFlowGraph location, asserts if out of range.
		*/
		inline RegSet get_def_ins(const location_type& loc) const {
			auto rs = def_ins_[loc.first];
			for ( int i = blocks_[loc.first], ie = i + loc.second; i < ie; ++i ) {
				const auto& instr = code_[i];
				rs |= instr.write_set();
				rs -= instr.undef_set();
			}
			return rs;
		}

		/** Returns the set of registers definitely defined at a code point.
			  @param idx Instruction index in underlying code, asserts if out of range.
		*/
		inline RegSet get_def_ins(size_t idx) const {
			return get_def_ins(get_location(idx));
		}

		/** Returns the set of registers definitely defined on exit from an 
			  instruction.
			  @param loc ControlFlowGraph location, asserts if out of range.
		*/
		inline RegSet get_def_outs(const location_type& loc) const {
			auto rs = get_def_ins(loc);
			const auto& instr = code_[get_index(loc)];
			rs |= instr.write_set();
			rs -= instr.undef_set();

			return rs;
		}

		/** Returns the set of registers definitely defined at a code point.
			  @param idx Instruction index in underlying code, asserts if out of range.
		*/
		inline RegSet get_def_outs(size_t idx) const {
			return get_def_outs(get_location(idx));
		}

		/** Returns true if the underlying code performs an undefined register read.
		*/
		inline bool performs_undef_read() const {
			size_t idx = 0;
			for ( size_t i = get_entry()+1, ie = get_exit(); i < ie; ++i ) {
				auto di = def_ins_[i];
				for ( size_t j = 0, je = num_instrs(i); j < je; ++j ) {
					const auto& instr = code_[idx++];
					const auto reads = instr.read_set();
					if ( (reads & di) != reads )
						return true;
					di |= instr.write_set();
					di -= instr.undef_set();
				}
			}
			return false;
		}

		/** Returns true if the underlying code passes all well-formedness checks.
		*/
		inline bool is_well_formed() const {
			return !performs_undef_read();
		}

		void write_dot(std::ostream& os) const;

	private:
		const Code& code_;
		RegSet inputs_;

		// This array records block begin/ends.
		// There are two empty blocks on either end and a sentinel value.
		// [ENTRY][b0 begin][b0 end/b1 begin]...[bn end/EXIT][SENTINEL]
		std::vector<size_t> blocks_;
		size_t num_blocks_;

		std::vector<std::vector<id_type>> preds_;
		std::vector<std::vector<id_type>> succs_;
		std::vector<id_type> exits_;

		// Dataflow values for each block
		std::vector<RegSet> live_ins_;
		std::vector<RegSet> live_outs_;

		std::vector<RegSet> def_ins_;
		std::vector<RegSet> def_outs_;

		// (this is the set of nodes that DOMINATE n)
		std::vector<std::bitset<64>> dom_ins_;
		std::vector<std::bitset<64>> dom_outs_;

		// Backedges and Loops
		std::vector<edge_type> back_edges_;
		std::map<edge_type, loop_type> loops_;

		// Nesting Depths
		std::vector<size_t> nesting_depth_;
};

} // namespace x64

#endif
