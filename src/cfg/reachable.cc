#include "src/cfg/reachable.h"

#include <stack>

using namespace std;

namespace x64 {

void Reachable::recompute() {
	stack<Cfg::id_type> r;
	r.push(cfg_.get_entry());

	while ( !r.empty() ) {
		const auto m = r.top();
		r.pop();
		for ( auto s = cfg_.succ_begin(m), se = cfg_.succ_end(m); s != se; ++s )
			if ( !cfg_.is_exit(*s) && reachable_.insert(*s).second )
				r.push(*s);
	}
}

} // namespace x64
