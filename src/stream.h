/*
Copyright 2013 eric schkufza

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

#ifndef X64ASM_SRC_STREAM_H
#define X64ASM_SRC_STREAM_H

#include <iostream>
#include <stdint.h>

#include "src/code.h"
#include "src/instruction.h"
#include "src/m.h"
#include "src/modifier.h"
#include "src/moffs.h"
#include "src/rel.h"
#include "src/sreg.h"
#include "src/st.h"

// iostream overloads.  Note that these are in the global namespace.

std::istream& operator>>(std::istream& is, x64asm::Code& c);

std::ostream& operator<<(std::ostream& os, const x64asm::Code& c);
std::ostream& operator<<(std::ostream& os, const x64asm::Instruction& i);
std::ostream& operator<<(std::ostream& os, const x64asm::M8& m);
std::ostream& operator<<(std::ostream& os, const x64asm::M16& m);
std::ostream& operator<<(std::ostream& os, const x64asm::M32& m);
std::ostream& operator<<(std::ostream& os, const x64asm::M64& m);
std::ostream& operator<<(std::ostream& os, const x64asm::M128& m);
std::ostream& operator<<(std::ostream& os, const x64asm::M256& m);
std::ostream& operator<<(std::ostream& os, const x64asm::M16Int& m);
std::ostream& operator<<(std::ostream& os, const x64asm::M32Int& m);
std::ostream& operator<<(std::ostream& os, const x64asm::M64Int& m);
std::ostream& operator<<(std::ostream& os, const x64asm::M32Fp& m);
std::ostream& operator<<(std::ostream& os, const x64asm::M64Fp& m);
std::ostream& operator<<(std::ostream& os, const x64asm::M80Fp& m);
std::ostream& operator<<(std::ostream& os, const x64asm::M80Bcd& m);
std::ostream& operator<<(std::ostream& os, const x64asm::FarPtr1616& m);
std::ostream& operator<<(std::ostream& os, const x64asm::FarPtr1632& m);
std::ostream& operator<<(std::ostream& os, const x64asm::FarPtr1664& m);
std::ostream& operator<<(std::ostream& os, const x64asm::Pref66& p);
std::ostream& operator<<(std::ostream& os, const x64asm::PrefRexW& p);
std::ostream& operator<<(std::ostream& os, const x64asm::Far& f);
std::ostream& operator<<(std::ostream& os, const x64asm::Moffs8& m);
std::ostream& operator<<(std::ostream& os, const x64asm::Moffs16& m);
std::ostream& operator<<(std::ostream& os, const x64asm::Moffs32& m);
std::ostream& operator<<(std::ostream& os, const x64asm::Moffs64& m);
std::ostream& operator<<(std::ostream& os, const x64asm::Rel8& r);
std::ostream& operator<<(std::ostream& os, const x64asm::Rel32& r);
std::ostream& operator<<(std::ostream& os, const x64asm::Sreg& s);
std::ostream& operator<<(std::ostream& os, const x64asm::Fs& s);
std::ostream& operator<<(std::ostream& os, const x64asm::Gs& s);
std::ostream& operator<<(std::ostream& os, const x64asm::St& s);
std::ostream& operator<<(std::ostream& os, const x64asm::St0& s);

#endif
