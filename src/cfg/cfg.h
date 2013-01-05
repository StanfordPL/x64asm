#ifndef X64_SRC_CFG_CFG_H
#define X64_SRC_CFG_CFG_H

#include <cassert>
#include <iostream>
#include <utility>
#include <vector>

#include "src/code/code.h"
#include "src/code/instruction.h"

namespace x64 {

/** A Control Flow Graph. */
class Cfg {
	public:
		typedef size_t id_type;
		typedef std::pair<id_type, size_t> location_type;

		Cfg(const Code& code) : code_{code} { 
			recompute();
		}

		void recompute();

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
			assert(id < num_blocks());
			// TODO... FINISH THIS
			return code_.rbegin();
		}

		inline instr_reverse_iterator instr_rend(id_type id) const {
			assert(id < num_blocks());
			// TODO... FINISH THIS
			return code_.rend();
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

	private:
		const Code& code_;

		// [ENTRY][b0 begin][b0 end/b1 begin]...[bn end/EXIT][SENTINEL]
		std::vector<size_t> blocks_;
		std::vector<std::vector<id_type>> preds_;
		std::vector<std::vector<id_type>> succs_;

		inline size_t get_index(const location_type& loc) const {
			assert(loc.first < num_blocks());
			assert(loc.second < num_instrs(loc.first));
			return blocks_[loc.first] + loc.second;
		}
};

} // namespace x64
#endif
