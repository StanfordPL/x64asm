#include "src/cfg/loops.h"

#include <stack>

using namespace std;

namespace x64 {

void Loops::recompute() {
	back_edges_.clear();
	for ( auto i = reachable_.begin(), ie = reachable_.end(); i != ie; ++i )
		for ( auto s = cfg_.succ_begin(*i), se = cfg_.succ_end(*i); s != se; ++s )
			if ( doms_.dom(*s, *i) )
				back_edges_.push_back(make_pair(*i, *s));

	nesting_depth_.resize(cfg_.num_blocks());
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

		stack<Cfg::id_type> s;
		s.push(e.first);

		while ( !s.empty() ) {
			const auto m = s.top();
			s.pop();

			for ( auto p = cfg_.pred_begin(m), pe = cfg_.pred_end(m); p != pe; ++p )
				if ( reachable_.contains(*p) && *p != e.second && l.insert(*p).second )
					s.push(*p);
		}

		for ( const auto bb : l )
			nesting_depth_[bb]++;
	}
}

} // namespace x64
