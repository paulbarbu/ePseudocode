module Tests.Scope (scopeTests)
where

import Test.HUnit

import EPseudocode.Parser (eParse, runParser, mainParser, toplevelParser)
import EPseudocode.Scope
import EPseudocode.Data

import Tests.Parser


scopeTest program =
  case isValidScope $ parseFile program of
    Left err -> assertFailure err
    Right () -> True @=? True


scopeTestFail program =
  case isValidScope $ parseFile program of
    Left _ -> True @=? True
    Right () -> assertFailure "The test passed"


scopeTests = TestList [
   "no main" ~: scopeTestFail "func foo() ret 1 sffunc"

 , "one main" ~: scopeTest "func main() ret 1 sffunc"

 , "double main" ~: scopeTestFail "func main() ret 1 sffunc func main() ret 1 sffunc"

 , "functions and assignment in global scope" ~: scopeTest "a=2 func x() ret 42 sffunc func main() ret 42 sffunc"

 , "wrong typecase for main func" ~: scopeTestFail "func mAIn() ret 42 sffunc"

 , "unique funcs" ~: scopeTest "func main() ret 42 sffunc func foo() ret 42 sffunc"

 , "duplicate funcs" ~: scopeTestFail "func main() ret 42 sffunc func foo() ret 42 sffunc func foo() ret 42 sffunc"

 , "main definition in main" ~: scopeTestFail "func main() func main() ret42 sffunc sffunc"

 , "undefined variable" ~: scopeTestFail "func main() a=x sffunc"

 , "defined variable x" ~: scopeTest "func main() x=5 a=x sffunc"

 , "recursive main" ~: scopeTest "func foo() ret 42 sffunc func main() main() sffunc"

 , "undefined function call in main" ~: scopeTestFail "func bar() ret 42 sffunc func main() func innerMain() ret 1 sffunc foo() sffunc"

 , "defined function call in main" ~: scopeTest "func bar() ret 42 sffunc func main() func innerMain() ret 1 sffunc bar() sffunc"

 , "undefined reference at the end" ~: scopeTestFail "func main() func a() ret 1 sffunc a() main() x sffunc"

 , "undefined reference in the middle" ~: scopeTestFail "func main() func a() ret 1 sffunc a() x main() sffunc"

 , "double definition and local function" ~:
    scopeTestFail "func foo() ret42 sffunc func foo() ret 42 sffunc func main() func foo() ret 1 sffunc ret 2 sffunc"

 , "single definition and local function" ~: scopeTest "func foo() ret 1 sffunc func main() func foo() ret 1 sffunc ret 42 sffunc"

 , "double main with local function" ~: scopeTestFail "func main() ret 42 sffunc func main() func foo() ret 1 sffunc sffunc"

 , "complete if introduces new scope" ~: scopeTestFail "func main() daca 1 atunci a=2 altfel a=3 sfdaca a sffunc"

 , "defined variable in complete if that is not referenced" ~: scopeTest "func main() daca 1 atunci a=2 altfel a=3 sfdaca sffunc"

 , "defined variable in complete if and referenced it" ~: scopeTest "func main() daca 1 atunci a=2 a altfel a=3 sfdaca sffunc"

 , "defined variable in complete if and referenced it on both branches" ~: scopeTest "func main() daca 1 atunci a=2 a altfel a=3 a sfdaca sffunc"

 , "undefined variable in complete if's condition" ~: scopeTestFail "func main() daca x == 1 atunci a=2 a altfel a=3 a sfdaca sffunc"

 , "defined variable in complete if's condition" ~: scopeTest "func main() x = 1 daca x <= 1 atunci a=2 a altfel a=3 a sfdaca sffunc"

 , "simple if introduces new scope" ~: scopeTestFail "func main() daca 1 atunci a=2 sfdaca a sffunc"

 , "defined variable in simple if that is not referenced" ~: scopeTest "func main() daca 1 atunci a=2 sfdaca sffunc"

 , "defined variable in simple if and referenced it" ~: scopeTest "func main() daca 1 atunci a=2 a sfdaca sffunc"

 , "undefined variable in simple if's condition" ~: scopeTestFail "func main() daca x >= 1 atunci a=2 a sfdaca sffunc"

 , "defined variable in simple if's condition" ~: scopeTest "func main() x = 1 daca x != 1 atunci x+2 !x x+2 sfdaca sffunc"

 , "valid func call in simple if's condition" ~: scopeTest "func foo() ret adevarat sffunc func main() daca foo() atunci ret 3 sfdaca sffunc"

 , "invalid func call in simple if's condition" ~: scopeTestFail "func main() daca foo() atunci ret 3 sfdaca sffunc"

 , "valid func call in complete if's condition" ~:
    scopeTest "func foo() ret adevarat sffunc func main() daca foo() atunci ret 3 altfel ret 1 sfdaca sffunc"

 , "invalid func call in complete if's condition" ~: scopeTestFail "func main() daca foo() atunci ret 3 altfel ret 1 sfdaca sffunc"

 , "valid func call in while's condition" ~: scopeTest "func foo() ret adevarat sffunc func main() cattimp foo() executa ret 3 sfcattimp sffunc"

 , "invalid func call in while's condition" ~: scopeTestFail "func main() cattimp foo() executa ret 3 sfcattimp sffunc"

 , "undefined variable in while's condition" ~: scopeTestFail "func main() cattimp !x executa ret 1 sfcattimp sffunc"

 , "defined variable in while's condition" ~: scopeTest "func main() x=2 y=0 cattimp x%2 == y executa x=x/2 sfcattimp sffunc"

 , "undefined list" ~: scopeTestFail "func main() daca a[2] atunci ret 1 altfel ret 3 sfdaca sffunc"

 , "variable in list index" ~: scopeTest "func main() b=1 a[b]=b ret a sffunc"

 , "undefined variable in list index" ~: scopeTestFail "func main() b=1 a[c]=2 ret a sffunc"

 , "variable in expression in list index" ~: scopeTest "func main() b=1 a[b % 2]=2  ret a sffunc"

 , "undefined variable in expression in list index" ~: scopeTestFail "func main() a[b + 1]=2  ret a sffunc"

 , "undefined variable after list index" ~: scopeTestFail "func main() b=1 a[b]=2 ret foo sffunc"

 , "access to undefined list" ~: scopeTestFail "func main() a={1,2,3} foo[2] sffunc"

 , "access to defined list" ~: scopeTest "func main() a={1,2,3} a[2] sffunc"

 , "access with undefined variable to defined list" ~: scopeTestFail "func main() a={1,2,3} a[b] sffunc"

 , "access with defined variable to defined list" ~: scopeTest "func main() b=1 a={1,2,3} a[b] sffunc"

 , "empty for with empty body" ~: scopeTest "func main() pt ;; executa sfpt sffunc"

 , "empty for with var access in body" ~: scopeTest "func main() a=2 pt ;; executa a=a+1 sfpt ret a sffunc"

 , "empty for with undefined var access in body" ~: scopeTestFail "func main() pt ;; executa a=a+1 sfpt sffunc"

 , "undefined variable in list index used in for initial" ~: scopeTestFail "func main() pt a[b]=2 ;; executa sfpt sffunc"

 , "defined variable in list index used in for initial and in body" ~: scopeTest "func main() pt a[1]=1;a[1]>1; executa ret a[1] sfpt sffunc"

 , "defined variable in for initial used in the body" ~: scopeTest "func main() pt a=2;; executa a sfpt sffunc"

 , "defined list index in for initial used in the body" ~: scopeTest "func main() a={1, 2} pt a[1]=2;; executa a[2] sfpt ret a sffunc"

 , "defined variable used in for condition" ~: scopeTest "func main() a=2 pt ;a>2; executa 1 sfpt sffunc"

 , "undefined variable used in for condition" ~: scopeTestFail "func main() pt ;a>2; executa 1 sfpt sffunc"

 , "undefined list index used in for condition" ~: scopeTestFail "func main() pt ;a[2]>2; executa 1 sfpt sffunc"

 , "defined list index used in for condition" ~: scopeTest "func main() a={1,2} b=5 pt ;a[2]>2; executa b=b+3 sfpt ret b sffunc"

 , "undefined variable in for iter" ~: scopeTestFail "func main() pt ;;a=a+1 executa sfpt sffunc"

 , "defined variable (in a higher scope) in for iter" ~: scopeTest "func main() a=1 pt ;;a=a+1 executa a=a+1 sfpt ret a sffunc"

 , "defined variable (in a higher scope) in for iter ad condition" ~: scopeTest "func main() a=1 pt ;a<=5;a=a+1 executa a=a+1 sfpt ret a sffunc"

 , "defined variable in for inital and used in for condition and for body" ~:
    scopeTest "func main() b=42 pt a=1;a<42; executa a=a+1 sfpt ret b sffunc"

 , "using already defined variable in for inital and in for condition and for body" ~:
    scopeTest "func main() a=5 pt a=a;a<42; executa a=a+1 sfpt ret a sffunc"

 , "undefined variable in for condition" ~: scopeTestFail "func main() pt a=1;b<42; executa 1 sfpt sffunc"

 , "defined variable in for initial and used in for iter and for body" ~:
    scopeTest "func main() b=42 pt a=1;;a=a+1 executa a=a+0 sfpt ret 42 sffunc"

 , "using already defined variable in for initial and in for iter and for body" ~:
    scopeTest "func main() i=5 pt a=i;;a=a+1 executa a=a+0 sfpt ret i sffunc"

 , "defined a variable in for initial and using another one in for condition" ~: scopeTestFail "func main() pt b=1;;a=a+1 executa 1 sfpt sffunc"

 , "undefined variable used in for condition and in iter" ~: scopeTestFail "func main() pt ;a>2;a=a+1 executa a=a+1 sfpt ret a sffunc"

 , "defined variable used in for condition and in iter" ~: scopeTest "func main() a=1 pt ;a<2;a=a+1 executa sfpt sffunc"

 , "fully blown for loop" ~: scopeTest "func main() pt i=0;i<42;i=i+1 executa i scrie(i) sfpt sffunc"

 , "fully blown for loop, separate condition" ~: scopeTest "func main() a=2 pt i=0;a<42;a=i+1 executa scrie(i) sfpt sffunc"

 , "fully blown for loop, undefined variable in cond and iter" ~: scopeTestFail "func main() b=0 pt i=0;a<42;a=i+1 executa scrie(i) sfpt sffunc"

 , "fully blown for loop with statements after it" ~: scopeTest "func main() pt i=0;i<42;i=i+1 executa scrie(i) sfpt scrie(\"bye\") sffunc"

 , "unknown variable in ret" ~: scopeTestFail "func main() ret a sffunc"

 , "expression with variable in ret" ~: scopeTest "func main() a=3 ret a-2 daca adevarat atunci a=a+2 sfdaca sffunc"

 , "undefined function call in ret" ~: scopeTestFail "func main() ret foo() sffunc"

 , "function call in ret" ~: scopeTest "func foo() scrie(42) sffunc func main() ret foo() sffunc"

 , "lambda in ret" ~: scopeTest "func main() ret func() scrie(42) sffunc sffunc"

 , "undefined index access in ret" ~: scopeTestFail "func main() ret a[3] sffunc"

 , "index access in ret" ~: scopeTest "func main() a={1, 2, 3} ret a[2] sffunc"

 , "undefined arguments in func call" ~: scopeTestFail "func foo(x, y, z) ret 1 sffunc func main() foo(a, b, c) sffunc"

 , "defined arguments in func call" ~: scopeTest "func foo(x, y, z) ret 1 sffunc func main() a=2 b=3 c=1 foo(a, b, c) sffunc"

 , "immediate & variable arguments in func call" ~: scopeTest "func foo(x, y, z) ret 1 sffunc func main() b=3 c=1 foo(1, b, c) sffunc"

 , "func call & immediate & variable arguments in func call" ~:
    scopeTest "func direct() ret 1 sffunc func foo(x, y, z) ret 1 sffunc func main() c=1 foo(1, direct(), c) sffunc"

 , "undefined func call & immediate args in func call" ~:
    scopeTestFail "func foo(x, y, z) ret 1 sffunc func main() foo(1, none(), 1) sffunc"

 , "undefined func call in func call" ~:
    scopeTestFail "func foo(x, y, z) ret 1 sffunc func main() foo(none()) sffunc"

 , "func call with args & immediate & variable arguments in func call" ~:
    scopeTest "func direct(x) ret x sffunc func foo(x, y, z) ret 1 sffunc func main() c=1 foo(1, direct(2), c) sffunc"

 , "func call with undefined args & immediate & variable arguments in func call" ~:
    scopeTestFail "func direct(x) ret x sffunc func foo(x, y, z) ret 1 sffunc func main() c=1 foo(1, direct(a), c) sffunc"

 , "lambda argument in func call" ~:
    scopeTest "func foo(x, y, z) ret x(3) sffunc func main() c=1 foo(func(a) ret a+2 sffunc, 2, c) sffunc"

 , "undefined variable in list" ~: scopeTestFail "func main() a={1, b} sffunc"

 , "variable in list" ~: scopeTest "func main() b=1 a={1, b} sffunc"

 , "one variable in list" ~: scopeTest "func main() b=1 a={b} sffunc"

 , "one undefined variable in list" ~: scopeTestFail "func main() a={b} sffunc"

 , "undefined variable in list in list" ~: scopeTestFail "func main() a={1, {3, b}} sffunc"

 , "variable in list" ~: scopeTest "func main() b=1 a={1, b} sffunc"

 , "undefined function call and value in list" ~: scopeTestFail "func main() a={1, none()} sffunc"

 , "undefined function call in list" ~: scopeTestFail "func main() a={none()} sffunc"

 , "undefined function call in nested list" ~: scopeTestFail "func main() a={1, {none()}} sffunc"

 , "function call in list" ~: scopeTest "func foo() ret 1 sffunc func main() a={foo()} sffunc"

 , "value & function call in list" ~: scopeTest "func foo() ret 1 sffunc func main() a={\"string\", foo()} sffunc"

 , "value & function call in nested list" ~: scopeTest "func foo() ret 1 sffunc func main() a={\"string\", {foo()}} sffunc"

 , "chained list index with undefined variable" ~: scopeTestFail "func main() a={} a[1][2][b] sffunc"

 , "args visible in func body" ~: scopeTest "func main(argc, argv) ret argv[1] sffunc"

 , "name already used by a function" ~: scopeTestFail "func foo() ret 1 sffunc func main() foo=3 sffunc" -- TODO: move this to type tests

 , "duplicate param names" ~: scopeTestFail "func main(a, a) foo=3 sffunc"

 , "undefined function call whose definition is in a list" ~: scopeTestFail "func main() b[1]() sffunc"

 , "function call whose definition is in a list" ~: scopeTest "func main() a={func(x) ret x sffunc} b=0 a[0](b) {1, 2} a[0] a[b](3) sffunc"

 , "fizzbuzz program" ~:
    do c <- readFile "examples/fizzbuzz.epc"
       scopeTest c

 , "process a range of numbers with a callback function and a custom step" ~:
    do c <- readFile "examples/callback.epc"
       scopeTest c

 , "compute the n-th fibonacci number" ~:
    do c <- readFile "examples/fib.epc"
       scopeTest c

 , "display indices in a list along with values" ~:
    do c <- readFile "examples/lists.epc"
       scopeTest c

 , "greatest common divisor program" ~:
    do c <- readFile "examples/greatest_common_div.epc"
       scopeTest c

 , "simple closure program" ~:
    do c <- readFile "examples/closure.epc"
       scopeTest c

 , "global variable program" ~:
    do  c <- readFile "examples/global.epc"
        scopeTest c
 ]
