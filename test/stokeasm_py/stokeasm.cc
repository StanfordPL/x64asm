#include <iostream>
#include <sstream>
#include <stdint.h>
#include <Python.h>

#include "include/x64.h"

using namespace std;
using namespace x64;

static char att_hex_docs[] = 
"att_hex(): disassemble assembly, return hex\n";

static PyObject* att_hex(PyObject* self, PyObject* args)
{

  char* input_string;

  /* This is safe: python guarantees to allocate
     and free memory for the input string. */
  if ( PyArg_ParseTuple(args, "s", &input_string) == 0)
    return Py_BuildValue("s", "Couldn't parse string argument.");

  //printf("got input string: %s\n", input_string);
  Code code;

  istringstream iss(input_string);
  iss >> format(ATT) >> code;

  if ( cin.fail() ) {
    return Py_BuildValue("s", "Error reading input!");
  }

  Assembler assm;
  Function fxn;

  ostringstream oss;
  assm.write_hex(oss, code);
  oss.flush();
  return Py_BuildValue("s", oss.str().c_str());

}

static char att_exec_docs[] = 
"att_exec(): run a chunk of assembly, and produce output of eax.\n";

static PyObject* att_exec(PyObject* self, PyObject* args)
{

  char* input_string;

  /* This is safe: python guarantees to allocate
     and free memory for the input string. */
  if ( PyArg_ParseTuple(args, "s", &input_string) == 0)
    return Py_BuildValue("s", "Couldn't parse string argument.");

  //printf("got input string: %s\n", input_string);
  Code code;

  istringstream iss(input_string);
  iss >> format(ATT) >> code;

  if ( cin.fail() ) {
    return Py_BuildValue("s", "Error reading input!");
  }

  Assembler assm;
  Function fxn;

  //cout << "Calling function... " << endl;
  assm.assemble(fxn, code);
  uint64_t res = fxn();
  return Py_BuildValue("l", res);

}

static PyMethodDef stokeasm_funcs[] = {
  {"att_exec", (PyCFunction)att_exec,
    METH_VARARGS, att_exec_docs},
  {"att_hex", (PyCFunction)att_hex,
    METH_VARARGS, att_hex_docs},
  {NULL}
};

extern "C" {

  void initstokeasm(void)
  {
    Py_InitModule3("stokeasm", stokeasm_funcs,
        "Python interface for STOKE assembler.");
  }

}

