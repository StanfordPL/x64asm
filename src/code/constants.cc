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

// Eflags (eflag.h)
const Eflag cf{Constants::cf()};
const Eflag pf{Constants::pf()};
const Eflag af{Constants::af()};
const Eflag zf{Constants::zf()};
const Eflag sf{Constants::sf()};
const Eflag tf{Constants::tf()};
const Eflag if_{Constants::if_()};
const Eflag df{Constants::df()};
const Eflag of{Constants::of()};
const Eflag iopl0{Constants::iopl0()};
const Eflag iopl1{Constants::iopl1()};
const Eflag nt{Constants::nt()};
const Eflag rf{Constants::rf()};
const Eflag vm{Constants::vm()};
const Eflag ac{Constants::ac()};
const Eflag vif{Constants::vif()};
const Eflag vip{Constants::vip()};
const Eflag id{Constants::id()};

const vector<Eflag> eflags {{
	cf, pf, af, zf, sf, tf, if_, df, 
	of, iopl0, iopl1, nt, rf, vm, ac, vif, 
	vip, id
}};

// Immediates (imm.h)
const Zero zero{Constants::zero()};
const One one{Constants::one()};
const Three three{Constants::three()};

} // namespace x64
