#include "src/code/constants.h"

using namespace std;

namespace x64 {

// Constant Registers (cr.h)
const Cr0234 cr0{Constants::cr0()};	
const Cr0234 cr2{Constants::cr2()};	
const Cr0234 cr3{Constants::cr3()};	
const Cr0234 cr4{Constants::cr4()};
const Cr8 cr8{Constants::cr8()};

const vector<Cr> crs {{
	cr0, cr2, cr3, cr4, cr8
}};

const vector<Cr0234> cr0234s {{
	cr0, cr2, cr3, cr4
}};

// Debug Registers (dr.h)
const Dr dr0{Constants::dr0()};
const Dr dr1{Constants::dr1()};
const Dr dr2{Constants::dr2()};
const Dr dr3{Constants::dr3()};
const Dr dr4{Constants::dr4()};
const Dr dr5{Constants::dr5()};
const Dr dr6{Constants::dr6()};
const Dr dr7{Constants::dr7()};

const vector<Dr> drs {{
	dr0, dr1, dr2, dr3, dr4, dr5, dr6, dr7 
}};

} // namespace x64
