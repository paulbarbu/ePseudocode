ePseudocode
===========
ePseudocode stands for "electronic [pseudocode](https://en.wikipedia.org/wiki/Pseudocode)".

This is an interpreter for a language designed to be similar to the pseudocode that students write on paper.

A word of warning though: All the keywords in the language itself are in Romanian, not in English, since ePseudocode
was specifically designed for romanian students, but you're always welcome to fork the project and contribute.
(Only the keywords are in Romanian, comments, docs and other stuff is in English, inlcuding errors issued by the
compiler/parser since I didn't focus on translating strings from parsec)

Description
===========

TODO: lambdas (done), floating point, strings (done), lists (done), functions - capture environment(done), io, stdlib (math, string, list & vector operations), files
TODO: turing complete
TODO: debugger
TODO: scientific notation
TODO: unicode support
TODO: list slicing
TODO: Bitwise operators
TODO: LC3 backend
TODO: x86 backend
TODO: FFI and C libraries (interop?)
TODO: codeblocks, vim, kdevelop, visualstudio, eclipse? syntax highlighting
TODO: tail call optimization?

TODO: bound, uniplate library
TODO: ivory language
TODO: apply, curry

TODO: coursera

TODO: short-circuit the sau and si operators: "adevarat si ret 42"

TODO: do not assign to function name inside function OR create separate variable

Not supported: variadic functions

* Case sensitive
* Influenced by C (a feature because one of the reasons it exists is to provide a lower learning curve, next step being C)
* If no `main` function is defined then all code in a file is executed as if it was in a `main` function
* Variables are declared on the spot, calid names start with "_" or with letters and continue with leters and numbers
* Operators: `=`, `+`, `-`, `*`, `/`, `%`, `==`, `!=`, `<` , `>`, `<=`, `>=`, `!`, `sau` (or), `si` (and), TODO
* Keywords (English translation in parens): `cattimp` (while), `sfcattimp` (end while), `pt` (for), `sfpt` (end for),
    `func` (start function), `sffunc` (end function), `ret` (return), `daca` (if), `atunci` (then), `altfel` (else), `sfdaca` (end if),
    `adevarat` (true), `fals` (false), TODO
* Builtins: `lung` (length), `TODO` (to_upper), `TODO` (to_lower), `TODO`, (to_int('c'))
* Hexadecimal and octal notation: "0xABC", "0o17"


Demo:
* " 0xF +  1 + /*com/*inside comment*/ment*/   4 - 5 //comment" => 15

Scoping rules
=============
 * There is a single "main" function in the global scope
 * Functions with the same name cannot be defined more than once in the same scope
 * Parameter names must be unique and they create a variable in the current function's scope
 * When a function call is made or a variable name encountered this must already exist
 (the function is searched in the list constructed in the 1st pass while the variable name should've
  been declared - assigned to - previously in the current scope)

Typing rules
============
* Function names cannot be used as variable names

Examples
========

TODO: ...

For more see the `examples` directory.

TODO: raytracer: http://fabiensanglard.net/rayTracing_back_of_business_card/

* List operations:
```
l = {1, "abc", 3, 2+6}
scrie(lung(l)) //outputs 4

//indexing (0-based indexing):
l = {1, 42, 3, 4}
scrie(l[1]) // outputs 42

//join two lists to get a new one:
l = l + {11} //append 11 to l
l += {12, 13} //apend 12 and 13 to l (`{1, 42, 3, 4, 11}`)
scrie(lung(l)) //outputs 7 (4 elements in the initial list + the appended 11 + the appended 12 and 13)

{1,2,3} - (1+2) => {1,2}

TODO: this is not valid: l += 1 //runtime error, cannot use '+' on list and string
```

* String operations:
```
//strings behave just like lists of characters
a = "foo\n\tbar"
scrie(lung(a)) // outputs 8
scrie(a[3]) //outputs a new line

a[3] = 'o' //assign the character o //TODO: maybe I can somehow sugar it to: a[3] = "o"
a[4] = 'b'
scrie(a) //outputs fooobbar

//concatenation (joining two list of characters to get a new one back)
scrie(a + "baz") // outputs foobaz

scrie(2 + "baz") // runtime error
scrie(2 + a) // runtime error

//string comparisons
"abc" == "ABC" //false
"abc" == "abc" //true
"a" <= "b" //true
"a" != "b" //true
```

Motivations
===========

* the use of an end statement (`sfdaca`, `sfcattimp`, `sfpt`) is enforced in order to avoid confusion as to where a statement ends
This is because I've seen beginners struggle with the fact that sometimes C/C++ permits the programmer to omit the curly braces
when a block contains a single statement.

License (BSD3)
==============
Copyright (c) 2014, Barbu Paul - Gheorghe

All rights reserved.
Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
