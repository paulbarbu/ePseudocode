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
 ]
