#include "src/io/dot_writer.h"

#include <algorithm>
#include <map>
#include <vector>

#include "src/cfg/cfg.h"
#include "src/cfg/dominators.h"
#include "src/cfg/loops.h"
#include "src/cfg/reachable.h"
#include "src/io/att_writer.h"

using namespace std;

namespace x64 {

void DotWriter::write(ostream& os, const Code& c) {
	Cfg cfg(c);
	Reachable reachable(cfg);
	Dominators dominators(cfg);
	Loops loops(cfg, dominators, reachable);

	os << "digraph g {" << endl;

	os << "colorscheme = blues6" << endl;

	os << "bb" << cfg.get_entry() << " [shape=box label=\"ENTRY\"];" << endl;
	os << "bb" << cfg.get_exit()  << " [shape=box label=\"EXIT\"];" << endl;

	map<size_t, vector<Cfg::id_type>> nestings;
	for ( size_t i = cfg.get_entry()+1, ie = cfg.get_exit(); i < ie; ++i )
		nestings[loops.nesting_depth(i)].push_back(i);

	for ( const auto& n : nestings ) {
		os << dec;
		os << "subgraph cluster_" << n.first << " {" << endl;
		os << "style = filled" << endl;
		os << "color = " << (n.first+1) << endl;

		for ( const auto bb : n.second ) {
			os << "bb" << dec << bb << "[";
			os << "shape=record, style=filled, fillcolor=white, ";
			if ( !reachable.contains(bb) )
				os << "color = grey, ";
			os << "label=\"{";
			os << "#" << bb; 
			os << "|live-ins:";
			os << "|live-outs:";
			os << "|";
			for ( size_t j = 0, je = cfg.num_instrs(bb); j < je; ++j ) {
				const auto& instr = cfg.get_instr(Cfg::location_type(bb,j));
				AttWriter::write(os, instr);
				os << "\\l";
			}
			os << "}\"];" << endl;
		}
	}
	for ( size_t i = 0, ie = nestings.size(); i < ie; ++i )
		os << "}" << endl;

	for ( size_t i = cfg.get_entry(), ie = cfg.get_exit(); i < ie; ++i ) 
		for ( auto s = cfg.succ_begin(i), se = cfg.succ_end(i); s != se; ++s ) {
			os << "bb" << dec << i << "->bb" << *s << " [";
			os << "style="; 
			if ( cfg.has_fallthrough_target(i) && 
					 cfg.get_fallthrough_target(i) == *s )
				os << "bold";
			else
				os << "dashed";
			os << " color=";
			if ( find(loops.back_edge_begin(), loops.back_edge_end(), 
						    Loops::edge_type(i,*s)) != loops.back_edge_end() )
				os << "red";
			else if ( reachable.contains(i) || cfg.is_entry(i) )
				os << "black";
			else
				os << "grey";
			os << "];" << endl;
		}

	os << "}";
}

} // namespace x64
