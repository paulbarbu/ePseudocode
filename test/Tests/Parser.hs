module Tests.Parser (parserTests)
where

import Test.HUnit

import EPseudocode.Parser (eParse)
import EPseudocode.Data


parse input = case eParse input of
    Left err -> error err
    Right expr -> expr


parserTests = TestList [
   "assign list to the variable a" ~:
    [Assign "a" (E (List [E (Int 1)]))] ~=? parse "a={1}"

 , "assign int to the variable a" ~:
    [Assign "a" (E (Int 1))] ~=? parse "a=1"

 , "function call" ~:
    [E (FuncCall (Var "a") [[]])] ~=? parse "a()"

 , "multiple indexing a list" ~:
    [E (Index "a" [Int 1,Int 2,Int 3,Int 4])] ~=? parse "a[1][2][3][4]"

 , "function call with function calls as args" ~:
    [E (FuncCall (Var "a") [[E (FuncCall (Var "b") [[]]),E (FuncCall (Var "c") [[]])]])] ~=? parse "a(b(), c())"

 , "function call with list index arg" ~:
    [E (FuncCall (Var "scrie") [[E (Index "a" [Int 2])]])] ~=? parse "scrie(a[2])"

 , "simple if and assignment" ~:
    [SimpleIf (Var "a") [Assign "a" (E (BinExpr Plus (Var "a") (Int 2)))]] ~=? parse "daca a atunci a=a+2 sfdaca"

 , "simple if with index condition" ~:
    [SimpleIf (Index "a" [Int 2]) [Assign "a" (E (BinExpr Plus (Var "a") (Int 2)))]] ~=? parse "daca a[2] atunci a=a+2 sfdaca"

 , "simple func def" ~:
    [FuncDef "x" [] [Ret (E (Int 3))]] ~=? parse "func x() ret 3 sffunc"

 , "assign function (returns function call) to variable" ~:
    [Assign "a" (FuncDef "x" [] [Ret (E (FuncCall (Var "y") [[]]))])] ~=? parse "a=func x() ret y() sffunc"

 , "list containing lambda" ~:
    [E (List [E (Int 1),E (Int 5),FuncDef "" [] [Ret (E (Int 42))]])] ~=? parse "{1, 5, func() ret 42 sffunc}"

 , "simple 'for' program" ~:
    do c <- readFile "examples/for.epc"
       [Assign "sum" (E (Int 0)),
        For (Assign "i" (E (Int 1))) (BinExpr Le (Var "i") (Int 100)) (Assign "i" (E (BinExpr Plus (Var "i") (Int 1))))
            [Assign "sum" (E (BinExpr Plus (Var "sum") (Var "i")))],
        E (FuncCall (Var "scrie") [[E (String "Sum = "),E (Var "sum")]])] @=? parse c

 , "simple 'hello world' program" ~:
    do c <- readFile "examples/hello.epc"
       [E (FuncCall (Var "scrie") [[E (String "Hello world")]])] @=? parse c

 , "process a range of numbers with a callback function and a custom step" ~:
    do c <- readFile "examples/callback.epc"
       [FuncDef "applyToRange" ["a","b","step","f"]
                [For (Assign "i" (E (Var "a"))) (BinExpr Le (Var "i") (Var "b")) (Assign "i" (E (FuncCall (Var "step") [[E (Var "i")]])))
                     [E (FuncCall (Var "f") [[E (Var "i")]])]],
        FuncDef "main" []
                [E (FuncCall (Var "applyToRange")
                    [[E (Int 1),E (Int 5),
                      FuncDef "" ["x"]
                              [Ret (E (BinExpr Plus (Var "x") (Int 1)))],
                      FuncDef "" ["x"]
                              [E (FuncCall (Var "scrie") [[E (BinExpr Mul (Var "x") (Int 2))]])]]])]] @=? parse c

 , "compute the n-th fibonacci number" ~:
    do c <- readFile "examples/fib.epc"
       [FuncDef "fib" ["n"]
                [SimpleIf (BinExpr Le (Var "n") (Int 2))
                          [Ret (E (Int 1))],
                Ret (E (BinExpr Plus (FuncCall (Var "fib") [[E (BinExpr Minus (Var "n") (Int 1))]])
                                     (FuncCall (Var "fib") [[E (BinExpr Minus (Var "n") (Int 2))]])))],
        FuncDef "main" [] [E (FuncCall (Var "scrie") [[E (FuncCall (Var "fib") [[E (Int 10)]])]])]] @=? parse c

 , "display indices in a list along with values" ~:
    do c <- readFile "examples/lists.epc"
       [Assign "l" (E (List [E (Int 1),E (Int 2),E (Int 5),E (Int 7),E (Int 8)])),
        For (Assign "i" (E (Int 0))) (BinExpr Lt (Var "i") (FuncCall (Var "lung") [[E (Var "l")]])) (Assign "i" (E (BinExpr Plus (Var "i") (Int 1))))
            [E (FuncCall (Var "scrie") [[E (Var "i"),E (Index "l" [Var "i"])]])]] @=? parse c

 , "greatest common divisor program" ~:
    do c <- readFile "examples/greatest_common_div.epc"
       [FuncDef "greatestCommonDivisor" ["a","b"]
                [While (BinExpr Neq (Var "a") (Var "b"))
                       [CompleteIf (BinExpr Gt (Var "a") (Var "b"))
                                   [Assign "a" (E (BinExpr Minus (Var "a") (Var "b")))]
                                   [Assign "b" (E (BinExpr Minus (Var "b") (Var "a")))]],
                 Ret (E (Var "a"))],
        FuncDef "gcd" ["a","b"]
                [SimpleIf (BinExpr Eq (Var "b") (Int 0))
                          [Ret (E (Var "a"))],
                 E (FuncCall (Var "gcd") [[E (Var "b"),E (BinExpr Mod (Var "a") (Var "b"))]])],
        FuncDef "main" []
                [E (FuncCall (Var "scrie") [[E (FuncCall (Var "gcd") [[E (Int 5),E (Int 25)]])]]),
                 E (FuncCall (Var "scrie") [[E (FuncCall (Var "greatestCommonDivisor") [[E (Int 3),E (Int 5)]])]])]] @=? parse c

 , "simple closure program" ~:
    do c <- readFile "examples/closure.epc"
       [FuncDef "plusN" ["n"]
                [Ret (FuncDef "" ["x"]
                              [Ret (E (BinExpr Plus (Var "n") (Var "x")))])],
        FuncDef "main" []
                [Assign "plus3" (E (FuncCall (Var "plusN") [[E (Int 3)]])),
                 E (FuncCall (Var "scrie") [[E (FuncCall (Var "plus3") [[E (Int 5)]])]])]] @=? parse c

  , "fizzbuzz program" ~:
    do c <- readFile "examples/fizzbuzz.epc"
       [Assign "n" (E (FuncCall (Var "citeste") [[]])),
        Assign "i" (E (Int 1)),
        While (BinExpr Le (Var "i") (Var "n"))
              [CompleteIf (BinExpr And
                                    (BinExpr Eq (BinExpr Mod (Var "i") (Int 3)) (Int 0))
                                    (BinExpr Eq (BinExpr Mod (Var "i") (Int 5)) (Int 0)))
                          [E (FuncCall (Var "scrie") [[E (String "fizzbuzz")]])]
                          [CompleteIf (BinExpr Eq (BinExpr Mod (Var "i") (Int 3)) (Int 0))
                                      [E (FuncCall (Var "scrie") [[E (String "fizz")]])]
                                      [CompleteIf (BinExpr Eq (BinExpr Mod (Var "i") (Int 5)) (Int 0))
                                                  [E (FuncCall (Var "scrie") [[E (String "buzz")]])]
                                                  [E (FuncCall (Var "scrie") [[E (Var "i")]])]]],
               Assign "i" (E (BinExpr Plus (Var "i") (Int 1)))]] @=? parse c

 , "imbricated ifs" ~:
    [SimpleIf (Int 1)
              [SimpleIf (Int 2)
                        [Ret (E (Int 3))],
               SimpleIf (Int 42)
                        [Ret (E (Int 5))]]] ~=? parse "daca 1 atunci daca 2 atunci ret 3 sfdaca daca 42 atunci ret 5 sfdaca sfdaca"

 , "sequential ifs (complete, simple)" ~:
    [CompleteIf (Int 1)
                [Ret (E (Int 2))]
                [Ret (E (Int 3))],
     SimpleIf (Int 42)
              [Ret (E (Int 5))]] ~=? parse "daca 1 atunci ret 2 altfel ret 3 sfdaca daca 42 atunci ret 5 sfdaca"

 , "function with sequential simple ifs" ~:
    [FuncDef "x" []
             [SimpleIf (Int 1)
                       [Ret (E (Int 2))],
              SimpleIf (Int 3)
                       [Ret (E (Int 42))]]] ~=? parse "func x() daca 1 atunci ret 2 sfdaca daca 3 atunci ret 42 sfdaca sffunc"

 , "lists and function calls passed to each other" ~:
    [E (List [E (Int 1),E (Int 2),
              E (FuncCall (Var "a") [[E (List [E (Int 3),E (Int 4),
                                               E (FuncCall (Var "b") [[]])]),
              E (Int 5)]])])] ~=? parse "{1, 2, a({3, 4, b()}, 5)}"

 , "function returns function" ~:
    [FuncDef "a" []
             [Ret (FuncDef "" []
                           [Ret (E (Int 1))])]] ~=? parse "func a() ret func() ret 1 sffunc sffunc"

 , "function call with anon function as arg" ~:
    [E (FuncCall (Var "a") [[FuncDef "" []
                                     [Ret (E (Int 42))]]])] ~=? parse "a(func() ret 42 sffunc)"
 ]
