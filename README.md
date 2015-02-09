ePseudocode
===========
ePseudocode stands for "electronic [pseudocode](https://en.wikipedia.org/wiki/Pseudocode)".

This is an interpreter for a language designed to be similar to the pseudocode that students write on paper.

A word of warning though: All the keywords in the language itself are in Romanian, not in English, since ePseudocode
was specifically designed for romanian students, but you're always welcome to fork the project and contribute.
(Only the keywords are in Romanian, comments, docs and other stuff is written in English, including errors issued by the
compiler/parser since I didn't focus on translating strings)

Description
===========

* Case sensitive
* Influenced by C (a feature because one of the reasons it exists is to provide a lower learning curve, next step being C)
* Variables are declared on the spot, valid names start with "_" or with letters and continue with leters and numbers
* Operators: `=`, `+`, `-`, `*`, `/`, `%`, `==`, `!=`, `<` , `>`, `<=`, `>=`, `!`, `sau` (or), `si` (and), `**` (pow), `.` (member access)
* Keywords (English translation in parens): `cattimp` (while), `executa` (do), `sfcattimp` (end while), `pt` (for), `sfpt` (end for),
    `func` (start function), `sffunc` (end function), `ret` (return), `daca` (if), `atunci` (then), `altfel` (else), `sfdaca` (end if),
    `adevarat` (true), `fals` (false), `stop` (break), `continua` (contiue), `struct` (structure declaration), `sfstruct` (end structure declaration)
* Builtins: `lung` (length), `int` (convert string to int), `float` (conver string to float),
    `floor`, `ceiling`, `sleep`, `apply`, `scrie` (printStr), `citeste`, (readStr), `deschide`
     (fileOpen), `inchide` (fileClose), `fscrie` (filePrintStr), `fciteste` (fileRead),
     `concat` (concatenate)
* Hexadecimal and octal notation: "0xABC", "0o17"

Not supported: variadic functions

Examples
========

For more complete samples see the `examples` directory.

* List operations:
```
l = {1, "abc", 3, 2+6}
scrie(lung(l)) //outputs 4

//indexing (0-based indexing):
l = {1, 42, 3, 4}
scrie(l[1]) // outputs 42

//add elemendts to a list
l = l + 11 + 12 + 13//append 11, 12, and 13 to l
scrie(lung(l)) //outputs 7 (4 elements in the initial list + the appended 11 + the appended 12 and 13)

{1,2,3} - (1+2) => {1,2}
```

* String operations:
```
//strings behave just like lists of characters
a = "foo\n\tbar"
scrie(lung(a)) // outputs 8
scrie(a[3]) //outputs a new line

a[3] = "o"
a[4] = "b"
scrie(a) //outputs fooobbar

//concatenation (joining two list of characters to get a new one back)
scrie(a + "baz") // outputs fooobbarbaz

scrie(2 + "baz") // "2baz"
scrie(2 + a) // "2fooobbar"

//string comparisons
"abc" == "ABC" //false
"abc" == "abc" //true
"a" <= "b" //true
"a" != "b" //true
```

Motivations
===========

* the use of an end statement (`sfdaca`, `sfcattimp`, `sfpt`, `sfstruct`, `sffunc`) is enforced in order to avoid confusion as to where a statement ends
This is because I've seen beginners struggle with the fact that sometimes C/C++ permits the programmer to omit the curly braces
when a block contains a single statement.

License (BSD3)
==============
Copyright (c) 2014-2015, Barbu Paul - Gheorghe

All rights reserved.
Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
