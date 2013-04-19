#ifndef TRACE_STATE_H
#define TRACE_STATE_H

#include <array>
#include <iostream>
#include <stdint.h>

namespace trace {

class State {
	public:
		std::array<uint64_t,16> general_purpose;
		std::array<__uint128_t,16> xmm;	
};

} // namespace trace

std::istream& operator>>(std::istream& is, trace::State& s);
std::ostream& operator<<(std::ostream& os, const trace::State& s);

#endif
