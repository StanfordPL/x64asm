#include <iostream>
#include <string>

#include "include/x64asm.h"

using namespace std;
using namespace x64asm;
using namespace cpputil;

typedef std::pair<x64asm::Opcode, std::vector<x64asm::Type>> Entry;
typedef std::vector<Entry> Row;
typedef std::map<std::string, Row> Table;

Table intel_table = {
  #include "src/intel.table"
};

std::vector<Type> parse_operand(string s) {

  std::vector<Type> types;
  std::vector<size_t> sizes;

  stringstream ss(s);

  // Read/write attributes (optional)
  char c = ss.peek();
  switch(c) {
    case '\'':
    case '=':
    case '!':
    case '&':
      ss.get();
    break;
  }
  // Argument type
  c = ss.get();
  switch(c) {
    case 'a':
      types.push_back(Type::AL);
      types.push_back(Type::AX);
      types.push_back(Type::EAX);
      types.push_back(Type::RAX);
      types.push_back(Type::XMM_0);
      break;
    case 'c':
      types.push_back(Type::CL);
      break;
    case 'd':
      types.push_back(Type::DX);
      break;
    case 'f':
      types.push_back(Type::ST);
      types.push_back(Type::ST_0);
      break;
    case 'i':
    case 'I':
      types.push_back(Type::IMM_8);
      types.push_back(Type::IMM_16);
      types.push_back(Type::IMM_32);
      types.push_back(Type::IMM_64);
      break;
    case 'E':
      types.push_back(Type::M_8);
      types.push_back(Type::M_16);
      types.push_back(Type::M_32);
      types.push_back(Type::M_64);
      types.push_back(Type::M_128);
      types.push_back(Type::M_256);
      //no break; E is for memory or register
    case 'r':
    case 'B':
    case 'G':
    case 'O':
    case 'R':
      types.push_back(Type::R_8);
      types.push_back(Type::R_16);
      types.push_back(Type::R_32);
      types.push_back(Type::R_64);
      break;
    case 'M':
    case 'N':
      types.push_back(Type::M_8);
      types.push_back(Type::M_16);
      types.push_back(Type::M_32);
      types.push_back(Type::M_64);
      types.push_back(Type::M_128);
      types.push_back(Type::M_256);
    case 'W':
      types.push_back(Type::M_8);
      types.push_back(Type::M_16);
      types.push_back(Type::M_32);
      types.push_back(Type::M_64);
      types.push_back(Type::M_128);
      types.push_back(Type::M_256);
      // no break: XMM or memory
    case 'H':
    case 'L':
    case 'U':
    case 'V':
      types.push_back(Type::XMM_0);
      types.push_back(Type::XMM);
      types.push_back(Type::YMM);
      break;
    case 't':
      types.push_back(Type::ST_0);
      break;
    case 'J':
      types.push_back(Type::LABEL);
      break;
  }

  //argument sizes
  string size;
  ss >> size;
  if(size == "b") {
    sizes.push_back(8);
  } else if (size == "d") {
    sizes.push_back(16);
  } else if (size == "do") {
    sizes.push_back(256);
  } else if (size == "dq") {
    sizes.push_back(128);
  } else if (size == "fq") {
    sizes.push_back(256);
  } else if (size == "o") {
    sizes.push_back(8);
  } else if (size[0] == 'p') {
    sizes.push_back(128);
    sizes.push_back(256);
  } else if (size == "q") {
    sizes.push_back(64);
  } else if (size == "r") {
    sizes.push_back(64);
  } else if (size == "v") {
    sizes.push_back(16);
    sizes.push_back(32);
    sizes.push_back(64);
  } else if (size == "w") {
    sizes.push_back(16);
  } else if (size == "x") {
    sizes.push_back(128);
    sizes.push_back(256);
  } else if (size == "y") {
    sizes.push_back(32);
    sizes.push_back(64);
  } else if (size == "z") {
    sizes.push_back(16);
    sizes.push_back(32);
  } else if (size == "") {
    sizes.push_back(8);
    sizes.push_back(16);
    sizes.push_back(32);
    sizes.push_back(64);
  }

  vector<Type> result;
  for(Type t : types) {
    size_t width = 0;
    switch(t) {
      case Type::IMM_8:
      case Type::R_8:
      case Type::M_8:
      case Type::AL:
      case Type::CL:
        width = 8;
        break;

      case Type::IMM_16:
      case Type::R_16:
      case Type::M_16:
      case Type::AX:
      case Type::DX:
        width = 16;
        break;

      case Type::IMM_32:
      case Type::R_32:
      case Type::M_32:
      case Type::EAX:
        width = 32;
        break;

      case Type::IMM_64:
      case Type::R_64:
      case Type::M_64:
      case Type::RAX:
        width = 64;
        break;

      case Type::XMM:
      case Type::XMM_0:
        width = 128;
        break;

      case Type::YMM:
        width = 256;
        break;

      default:
        width = 0;
        break;
    }

    bool found = false;
    for(size_t size : sizes) {
      if (size == width) {
        found = true;
        break;
      }
    }
    if(found)
      result.push_back(t);
  }


  return result;

}

/** A simple test program. Reads att syntax and prints human readable hex. */
int main(int argc, char** argv) {
  string line;
  cout << "{";
  while(cin.good()) {
    getline(cin, line);
    
    if(line[0] == '#')
      continue;
    if(line[0] == '"')
      continue;
    if(line[0] == ' ')
      continue;

    if(!line.size())
      continue;

    line = line.substr(0, line.find_first_of(','));

    stringstream ss(line);
    string name;
    ss >> name;

    //cout << "Found " << name << endl;

    vector<Opcode> opcodes;
    vector<vector<Type>> operands;
    while(ss.good()) {
      string op;
      ss >> op;
      //cout << "  Read operand " << op << endl;
      vector<Type> operand_options = parse_operand(op);
      operands.push_back(operand_options);
      //cout << "  ";
      //for(Type u : operand_options) {
        //cout << "  " << (uint64_t)u;
      //}
      //cout << endl;
    }

    // Find all matching opcodes
    Row possible_encodings = intel_table[name];
    for(auto entry : possible_encodings) {
      //cout << "  ** Testing " << (uint64_t)entry.first << endl;
      if(entry.second.size() != operands.size()) {
        //cout << "     - wrong number of operands" << endl;
        continue;
      }

      bool entry_works = true;
      for(size_t i = 0; i < operands.size(); ++i) {
        bool operand_works = false;
        for(size_t j = 0; j < operands[i].size(); ++j) {
          if(operands[i][j] == entry.second[operands.size() - i - 1]) {
            operand_works = true;
            break;
          }
        }

        entry_works &= operand_works;

        if(!operand_works) {
          //cout << "     - operand " << i << "failed" << endl;
          continue;
        }
      }

      if(entry_works) {
        //cout << "  opcode " << (uint64_t)entry.first << endl;
        cout << " " << entry.first;
        opcodes.push_back(entry.first);
      }
    }


  }
  cout << " }" << endl;

  return 0;
}
