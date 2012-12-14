#ifndef X64_SRC_CODE_MODIFIER_H
#define X64_SRC_CODE_MODIFIER_H

namespace x64 {

/** An instruction modifier. Used to disambiguate otherwise identical
	  instruction signatures.
*/
class Modifier {
	public:
		Modifier() { }
	private:
		int ignore_;
};

/** The 32-bit memory address override prefix: 0x66. */
struct Pref66 : public Modifier {
	Pref66() : Modifier{} { }
};

/** The REX.w prefix: 0x48. */
struct PrefRexW : public Modifier {
	PrefRexW() : Modifier{} { }
};

/** Far instruction variant. */
struct Far : public Modifier {
	Far() : Modifier{} { }
};

extern const Pref66 pref_66;
extern const PrefRexW pref_rexw;
extern const Far far;

} // namespace x64

#endif
