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

-------------------------------------------------------------------------------

trace

trace is a simple example of how the x64asm library can be used to assemble
an instrumented function to print out the CPU state prior to every
instruction's execution. This is a work in progress. The current version does
not support functions which dereference memory.

-------------------------------------------------------------------------------

Getting started:

1. Build the code

Type make (optionally specify avx for machines with AVX support)

$ make (avx)

2. Edit the example input files

examples/code.s:  A simple memory-free function
examples/sse.cpu: An input state description for a machine without AVX support
examples/avx.cpu: An input state description for a machine with AVX support

3. Run the code

Choose a cpu file which is appropriate to your build. The contents of the linux
ABI input registers (%rdi, %rsi, %rcx, %rdx, %r8, %r9, %xmm0-%xmm7|%ymm0-%ymm7)
will be copied to the machine and the code executed. During execution, the
machine state will printed prior to every instruction's execution.

$ trace examples/code.s {examples/sse.cpu|examples/avx.cpu}

4. Use this code in some of your own projects.

-------------------------------------------------------------------------------

Known Issues:

See github's issue tracker.
