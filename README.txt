Build Instructions:

0. Upgrade to ubuntu 12.04 LTS.

1. Dependency hell!

$ sudo apt-get install svn
$ sudo apt-get install cabal
$ sudo apt-get install g++
$ sudo apt-get install g++-multilib
$ sudo apt-get install doxygen
$ sudo apt-get install ghc
$ sudo apt-get install graphviz
$ sudo apt-get install libghc-split-dev

$ sudo cabal install Split

Note that there are almost certainly more that I'm unaware of.

2. (Be connected to the internet and) type make.
   You can optionally specify a build type.

$ make (release|profile|debug)

3. Browse the woefully incomplete documentation.

$ <browser> doc/html/index.html

4. Write and compile some code.

// code.cc
#include "<path/to/here>/include/x64.h"

int main() {
	...
}

$ g++ code.cc -I<path/to/here> <path/to/here>/lib/x64.a

------------------------------------------------

x86-64 quirks:

 * esp/rsp cannot be used as an index for looking up stuff from memory.

 * g++ allows jumps to undefined labels.  
   These are emitted as a 32-bit relative displacement of 0.

	 .foo:      <nothing>
	 jns .bar   e9 00 00 00 00
	 retq       c3

 * Deciding between the 8- and 32-bit relative displacement forms is
   known as the branch displacement problem.
	 The primary consequence of algorithm choice seems to be code size.
	 I *think* that gcc uses an iterative algorithm which initially assumes
	 8 bit offsets and then adjusts as necessary.
	 For this go around, I think we'll always emit 32-bit in the interest of speed.
