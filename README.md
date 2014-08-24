ePseudocode
===========
ePseudocde stands for "electronic [pseudocode](https://en.wikipedia.org/wiki/Pseudocode)".

This is an interpreter for a language designed to be similar to the pseudocode that students write on paper.

A word of warning though: All the keywords in the language itself are in Romanian, not in English, since ePseudocode
was specifically designed for romanian students, but you're always welcome to fork the project and contribute.
(Only the keywords are in Romanian, comments, docs and other stuff is in English)

Features (or characteristics)
=============================

TODO: lambdas (done), floating point, strings (done), vectors, lists (done), functions (done), variadic functions, comments (nested), io, stdlib (math, string, list & vector operations), dictionaries
TODO: ?modules?
TODO: turing complete
TODO: codeblocks extension
TODO: debugger
TODO: scientific notation
TODO: unicode support

* Highly influenced by C (a feature because one of the reasons it exists is to provide a lower learning curve, next step being C)
* If no `main` function is defined then all code in a file is executed as if it was in a `main` function
* Variables are declard on the spot
* Operators: `=`, `+`, `+=`, `-`, `-=`, `*`, `*=`, `/`, `/=`, `%`, `%=`, `==`, `!=`, `<` , `>`, `<=`, `>=`, TODO
* Keywords (English translation in parens): `cat timp` (while), `sf cat timp` (end while), `pentru` (for), `sf pentru` (end for), `func` (start function), `sf func` (end function), `daca` (if), TODO
* Builtins: `lung` (length), `TODO` (to_upper), `TODO` (to_lower), `TODO`, (to_int('c'))

Examples
========

TODO: ...

For more see the `examples` directory.

TODO: compile in itself

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

l += 1 //runtime error, cannot use '+' on list and string
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

* the use of an end statement (`sf daca`, `sf cat timp`, `sf pentru`) is enforced in order to avoid confusion as to where a statement ends
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
