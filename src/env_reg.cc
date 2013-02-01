/*
Copyright 2103 eric schkufza

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

#include "env_reg.h"

using namespace std;

namespace x64asm {

EnvReg::~EnvReg() {
	// Does nothing.
}

void FpuData::write_att(ostream& os) const {
	os << "%";
	write_intel(os);
}

void FpuData::write_intel(ostream& os) const {
	os << "fpu_data";
}

void FpuInstruction::write_att(ostream& os) const {
	os << "%";
	write_intel(os);
}

void FpuInstruction::write_intel(ostream& os) const {
	os << "fpu_instruction";
}

void FpuOpcode::write_att(ostream& os) const {
	os << "%";
	write_intel(os);
}

void FpuOpcode::write_intel(ostream& os) const {
	os << "fpu_opcode";
}

void Rip::write_att(ostream& os) const {
	os << "%";
	write_intel(os);
}

void Rip::write_intel(ostream& os) const {
	os << "rip";
}

} // namespace x64asm
