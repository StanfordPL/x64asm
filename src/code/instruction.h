#ifndef X64_SRC_CODE_INSTRUCTION_H
#define X64_SRC_CODE_INSTRUCTION_H

#include <cassert>
#include <initializer_list>
#include <vector>

#include "src/code/opcode.h"
#include "src/operands/operand.h"

namespace x64 {

/** A single hardware instruction.
		This representation is not typesafe and should be verified
			prior to assembly.
*/
class Instruction {
	public:
		inline Instruction(Opcode opcode) 
				: opcode_(opcode), operands_{{0,0,0,0}} { 
		}

		inline Instruction(Opcode opcode, 
				               std::initializer_list<Operand> operands)
				: opcode_(opcode), operands_(operands.begin(), operands.end()) {
			assert(operands.size() <= 4);
		}

		template <typename InItr>
		inline Instruction(Opcode opcode, InItr begin, InItr end) 
				: opcode_(opcode), operands_(begin, end) {
			assert(end-begin <= 4);		
			std::copy(begin, end, operands_.begin());
		}

		inline Opcode get_opcode() const {
			return opcode_;
		}

		inline void set_opcode(Opcode o) {
			opcode_ = o;
		}

		inline void set_operand(Operand o, size_t index) {
			assert(index < operands_.size());
			operands_[index] = o;
		}	

		inline Operand get_operand(size_t index) const {
			assert(index < operands_.size());
			return operands_[index];
		}

	private:
		Opcode opcode_;
		std::vector<Operand> operands_;
};

} // namespace x64

#endif
