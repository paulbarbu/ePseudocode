ePseudocode
===========
(work in progress)
ePseudocode stands for "electronic [pseudocode](https://en.wikipedia.org/wiki/Pseudocode)".

This is an interpreter for a language designed to be similar to the pseudocode that students write on paper.

A word of warning though: All the keywords in the language itself are in Romanian, not in English, since ePseudocode
was specifically designed for romanian students, but you're always welcome to fork the project and contribute.
(Only the keywords are in Romanian, comments, docs and other stuff is in English, including errors issued by the
compiler/parser since I didn't focus on translating strings from parsec)

Description
===========
* Case sensitive
* Influenced by C (a feature because one of the reasons it exists is to provide a lower learning curve, next step being C)
* Variables are declared on the spot, valid names start with "_" or with letters and continue with leters and numbers
* Operators (English translation in parens): `=`, `+`, `-`, `*`, `/`, `%`, `==`, `!=`, `<` , `>`, `<=`, `>=`, `!`, `sau` (or), `si` (and)
* Keywords (English translation in parens): `cattimp` (while), `sfcattimp` (end while), `pt` (for), `sfpt` (end for),
    `func` (start function), `sffunc` (end function), `ret` (return), `daca` (if), `atunci` (then), `altfel` (else), `sfdaca` (end if),
    `adevarat` (true), `fals` (false)
* Builtins: `lung` (length), `litere_mari` (to_upper), `litere_mici` (to_lower)
* Hexadecimal and octal notation: "0xABC", "0o17"

Scoping rules
=============
 * There is a single "main" function in the global scope
 * Functions with the same name cannot be defined more than once in the same scope
 * Function names cannot be used as variable names
 * Functions are discovered on an initial pass through the respective scope
 * Parameter names must be unique and they create a variable in the current funcion's scope
 * Variables inside a function cannot have the same name as the function in which they exist (the same applies for functions)
 * When a function call is made or a variable name encountered this must already exist
 (the function is searched in the list constructed in the 1st pass
 while the variable name should've been declared - assigned to - previously in the current scope)

Examples
========

For more see the `examples` directory.

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
