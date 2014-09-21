module Tests.Parser (parserTests)
where

import Data.List (isPrefixOf, isInfixOf)

import Test.HUnit

import EPseudocode.Parser (eParse, runParser)
import EPseudocode.Lexer
import EPseudocode.Data


parse input = case eParse input of
    Left err -> error err
    Right expr -> expr


parseFail input needle = case eParse input of
    Left err -> needle `isInfixOf` err @? "parser succeeded: " ++ err
    Right ast -> error $ "parser succeeded with: " ++ show ast

{- TODO: improve error messages:
"pt " -- do not use the mainParser there, instead just the specific ones
"pt 1;2;3;"
"daca 2 atunci ret 2 altfel sf" --because the sfdaca is not complete the error si ambigous
"daca a atunci altfel a( sfdaca" -- the error is on the "else" branch but the keyword "altfel" is blamed
"daca 2 atucni sfdaca" -- unexpected "a"
-}

parserTests = TestList [
   "assign list to the variable a" ~:
    [Assign (Var "a") (E (List [E (Int 1)]))] ~=? parse "a={1}"

 , "assign int to the variable a" ~:
    [Assign (Var "a") (E (Int 1))] ~=? parse "a=1"

 , "function call" ~:
    [E (FuncCall (Var "a") [[]])] ~=? parse "a()"

 , "multiple indexing a list" ~:
    [E (Index "a" [Int 1,Int 2,Int 3,Int 4])] ~=? parse "a[1][2][3][4]"

 , "function call with function calls as args" ~:
    [E (FuncCall (Var "a") [[E (FuncCall (Var "b") [[]]),E (FuncCall (Var "c") [[]])]])] ~=? parse "a(b(), c())"

 , "function call with list index arg" ~:
    [E (FuncCall (Var "scrie") [[E (Index "a" [Int 2])]])] ~=? parse "scrie(a[2])"

 , "simple if and assignment" ~:
    [SimpleIf (BinExpr Ge (Var "a") (Int 2))
              [Assign (Var "a") (E (BinExpr Plus (Var "a") (Int 2)))]] ~=? parse "daca a>=2 atunci a=a+2 sfdaca"

 , "simple if with index condition" ~:
    [SimpleIf (Index "a" [Int 2])
              [Assign (Var "a") (E (BinExpr Plus (Var "a") (Int 2)))]] ~=? parse "daca a[2] atunci a=a+2 sfdaca"

 , "simple func def" ~:
    [FuncDef "x" [] [Ret (E (Int 3))]] ~=? parse "func x() ret 3 sffunc"

 , "assign function (returns function call) to variable" ~:
    [Assign (Var "a") (FuncDef "x" [] [Ret (E (FuncCall (Var "y") [[]]))])] ~=? parse "a=func x() ret y() sffunc"

 , "list containing lambdas" ~:
    [E (List [E (Int 1),E (Int 5),FuncDef "" [] [Ret (E (Int 42))],FuncDef "" [] [Ret (E (Int 43))]])] ~=? parse "{1, 5, func() ret 42 sffunc, func() ret 43 sffunc}"

 , "simple 'for' program" ~:
    do c <- readFile "examples/for.epc"
       [Assign (Var "sum") (E (Int 0)),
        For (Just (Assign (Var "i") (E (Int 1)))) (Just (BinExpr Le (Var "i") (Int 100))) (Just (Assign (Var "i") (E (BinExpr Plus (Var "i") (Int 1)))))
            [Assign (Var "sum") (E (BinExpr Plus (Var "sum") (Var "i")))],
        E (FuncCall (Var "scrie") [[E (String "Sum = "),E (Var "sum")]])] @=? parse c

 , "simple 'hello world' program" ~:
    do c <- readFile "examples/hello.epc"
       [E (FuncCall (Var "scrie") [[E (String "Hello world")]])] @=? parse c

 , "process a range of numbers with a callback function and a custom step" ~:
    do c <- readFile "examples/callback.epc"
       [FuncDef "applyToRange" ["a","b","step","f"]
                [For (Just (Assign (Var "i") (E (Var "a")))) (Just (BinExpr Le (Var "i") (Var "b"))) (Just (Assign (Var "i") (E (FuncCall (Var "step") [[E (Var "i")]]))))
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
       [Assign (Var "l") (E (List [E (Int 1),E (Int 2),E (Int 5),E (Int 7),E (Int 8)])),
        For (Just (Assign (Var "i") (E (Int 0)))) (Just (BinExpr Lt (Var "i") (FuncCall (Var "lung") [[E (Var "l")]]))) (Just (Assign (Var "i") (E (BinExpr Plus (Var "i") (Int 1)))))
            [E (FuncCall (Var "scrie") [[E (Var "i"),E (Index "l" [Var "i"])]])]] @=? parse c

 , "greatest common divisor program" ~:
    do c <- readFile "examples/greatest_common_div.epc"
       [FuncDef "greatestCommonDivisor" ["a","b"]
                [While (BinExpr Neq (Var "a") (Var "b"))
                       [CompleteIf (BinExpr Gt (Var "a") (Var "b"))
                                   [Assign (Var "a") (E (BinExpr Minus (Var "a") (Var "b")))]
                                   [Assign (Var "b") (E (BinExpr Minus (Var "b") (Var "a")))]],
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
                [Assign (Var "plus3") (E (FuncCall (Var "plusN") [[E (Int 3)]])),
                 E (FuncCall (Var "scrie") [[E (FuncCall (Var "plus3") [[E (Int 5)]])]])]] @=? parse c

  , "fizzbuzz program" ~:
    do c <- readFile "examples/fizzbuzz.epc"
       [Assign (Var "n") (E (FuncCall (Var "citeste") [[]])),
        Assign (Var "i") (E (Int 1)),
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
               Assign (Var "i") (E (BinExpr Plus (Var "i") (Int 1)))]] @=? parse c

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

 , "function with sequential if and return" ~:
    [FuncDef "x" []
             [Assign (Var "a") (E (Bool False)),
              SimpleIf (Var "a")
                       [Assign (Var "a") (E (Bool True))],
              Ret (E (Int 42))]] ~=? parse "func x() a=fals daca a atunci a=adevarat sfdaca ret 42 sffunc"

 , "multiple function call - first function returned a function" ~:
    [E (FuncCall (Var "a") [[],[]])] ~=? parse "a()()"

 , "multiple function call - more general" ~:
    [E (FuncCall (Var "a") [[],[],[],[],[],[],[],[]])] ~=? parse "a()()()()()()()()"

 , "list index operation returns a function which is in turn called" ~:
    [E (FuncCall (Index "a" [Int 1]) [[]])] ~=? parse "a[1]()"

 , "list indexed by function call returns function that is in turn called" ~:
    [E (FuncCall (Index "a" [FuncCall (Var "foo") [[]]]) [[]])] ~=? parse "a[foo()]()"

 , "function call, one of the args being a lambda" ~:
    [E (FuncCall (Var "a") [[E (Int 1),FuncDef "" [] [Ret (E (Int 2))]]])] ~=? parse "a(1, func() ret 2 sffunc)"

 , "function call with other function calls as args" ~:
    [E (FuncCall (Var "a") [[E (FuncCall (Var "b") [[]]),E (FuncCall (Var "c") [[],[E (Int 1)]])]])] ~=? parse "a(b(), c()(1))"

 , "complete if with function calls" ~:
    [CompleteIf (FuncCall (Var "a") [[E (FuncCall (Var "b") [[]]),E (FuncCall (Var "c") [[]])]])
                [E (FuncCall (Var "a") [[E (FuncCall (Var "b") [[]]),E (FuncCall (Var "c") [[]])]])]
                [E (FuncCall (Var "a") [[E (Int 1),E (Int 2)]])]] ~=? parse "daca a(b(), c()) atunci a(b(), c()) altfel a(1, 2) sfdaca"

 , "function followed by if" ~:
    [FuncDef "x" []
             [SimpleIf (Int 1)
                       [Ret (E (Int 2))]],
     SimpleIf (Int 3)
              [Ret (E (Float 4.2))]] ~=? parse "func x() daca 1 atunci ret 2 sfdaca sffunc daca 3 atunci ret 4.2 sfdaca"


 , "return followed by return" ~:
    [FuncDef "x" []
             [Ret (FuncDef "y" []
                           [Ret (E (UnExpr UnMinus (Int 1)))]),
              Ret (E (Int 2))]] ~=? parse "func x() ret func y() ret -1 sffunc ret 2 sffunc"

 , "return followed by simple if" ~:
    [FuncDef "x" []
             [Ret (FuncDef "y" []
                           [Ret (E (Int 1))]),
              SimpleIf (Int 2)
                       [Ret (E (Int 3))]]] ~=? parse "func x() ret func y() ret 1 sffunc daca 2 atunci ret 3 sfdaca sffunc"

 , "simple if with sequence of statements" ~:
    [SimpleIf (UnExpr Not (Bool False))
              [Assign (Var "a") (E (BinExpr Mul (BinExpr Minus (Int 2) (Int 1)) (Int 3))),
               Assign (Var "b") (E (Int 3)),
               Assign (Var "c") (E (Int 4))]] ~=? parse "daca !fals atunci a=(2-1)*3 b=3 c=4 sfdaca"

 , "complete if with sequence of statements" ~:
    [CompleteIf (BinExpr Or (Int 1) (Int 1))
                [Assign (Var "a") (E (Int 2)),
                 Assign (Var "b") (E (Int 3)),
                 Assign (Var "c") (E (Int 4))]
                [Assign (Var "a") (E (Int 3)),
                 Assign (Var "b") (E (Int 42)),
                 Assign (Var "c") (E (Int 5))]] ~=? parse "daca 1 sau 1 atunci a=2 b=3 c=4 altfel a=3 b=42 c=5 sfdaca"

 , "for with sequence of statements" ~:
    [For (Just (Assign (Var "a") (E (Int 1)))) (Just (BinExpr Lt (Var "a") (Int 42))) (Just (Assign (Var "a") (E (BinExpr Plus (Var "a") (Int 1)))))
         [Assign (Var "b") (E (Int 3)),
          Assign (Var "c") (E (Var "a"))]] ~=? parse "pt a=1;a<42;a=a+1 executa b=3 c=a sfpt"

 , "while with sequence of statements" ~:
    [While (Bool True)
           [Assign (Var "a") (E (Int 1)),
            Assign (Var "b") (E (BinExpr Div (Int 2) (Int 4))),
            Assign (Var "c") (E (Int 3))]] ~=? parse "cattimp adevarat executa a=1 b=2/4 c=3 sfcattimp"

 , "function definition with sequence of statements" ~:
    [FuncDef "a" []
             [Assign (Var "b") (E (Int 3)),
              Assign (Var "c") (E (Int 4))]] ~=? parse "func a() b=3 c=4 sffunc"

 , "pow(positive int, positive int)" ~:
    [E (BinExpr Pow (Int 2) (Int 4))] ~=? parse "2 ** 4"

 , "-pow(positive int, positive int)" ~:
    [E (UnExpr UnMinus (BinExpr Pow (Int 2) (Int 4)))] ~=? parse "-2 ** 4"

 , "pow(negative expr, positive int)" ~:
    [E (BinExpr Pow (UnExpr UnMinus (Int 2)) (Int 4))] ~=? parse "(-2) ** 4"

 , "pow(negative expr, negative int)" ~:
    [E (BinExpr Pow (UnExpr UnMinus (Int 2)) (Int (-4)))] ~=? parse "(-2) ** -4"

 , "pow(negative expr, negative int) - no space" ~:
    [E (BinExpr Pow (UnExpr UnMinus (Int 2)) (Int (-4)))] ~=? parse "(-2) **-4"

 , "pow(negative expr, negative expr)" ~:
    [E (BinExpr Pow (UnExpr UnMinus (Int 2)) (UnExpr UnMinus (Int 4)))] ~=? parse "(-2) ** (-4)"

 , "pow(int , float)" ~:
    [E (BinExpr Pow (Int 4) (Float 2.5))] ~=? parse "4 ** 2.5"

 , "expression" ~:
    [E (BinExpr Minus (BinExpr Plus (BinExpr Plus (Int 15) (Int 1)) (Int 4)) (Int 5))] ~=? parse " 0xF +  1 + /*com/*inside comment*/ment*/   4 - 5 //comment"

 , "empty while" ~:
    [While (BinExpr Mod (Int 2) (Int 3)) []] ~=? parse "cattimp 2%3 executa sfcattimp"

 , "empty complete if" ~:
    [CompleteIf (Int 2) [] []] ~=? parse "daca 2 atunci altfel sfdaca"

 , "empty for" ~:
    [For (Just (Assign (Var "a") (E (Int 1))))
         (Just (BinExpr Le (Var "a") (Int 12)))
         (Just (Assign (Var "a") (E (BinExpr Plus (Var "a") (Int 3))))) []] ~=? parse "pt a=1;a<=12;a=a+3 executa sfpt"

 , "infinite empty for" ~:
    [For Nothing Nothing Nothing []] ~=? parse "pt ;; executa sfpt"

 , "empty simple if" ~:
    [SimpleIf (BinExpr Mod (Var "a") (Int 2)) []] ~=? parse "daca a%2 atunci sfdaca"

 , "assign to list index" ~:
    [Assign (Index "a" [Int 2]) (E (Int 3))] ~=? parse "a[2]=3"

 , "for with list indices" ~:
    [For (Just (Assign (Index "a" [Int 2]) (E (Int 1))))
         (Just (BinExpr Le (Index "a" [Int 2]) (Int 5)))
         (Just (Assign (Index "a" [Int 2]) (E (Int 3)))) []] ~=? parse "pt a[2]=1;a[2]<=5;a[2]=3 executa sfpt"

 , "run parser, simple expression" ~:
    "[E (Int 1)]" ~=? runParser "1"

 , "run parser, simple minus expression" ~:
    "[E (UnExpr UnMinus (Int 1))]" ~=? runParser "-1"

 , "run parser, simple expression" ~:
    "[E (FuncCall (Var \"a\") [[E (BinExpr Div (Int 5) (Int 2))]])]" ~=? runParser "a(5/2)"

 , "incomplete if and function call" ~: parseFail "daca a(" tThen

 , "incomplete function def" ~: parseFail "func a" "parameters list"

 , "incomplete parameter listf" ~: parseFail "func a()" tEndFunc

 , "incomplete while" ~: parseFail "cattimp adevarat executa" tDo

 , "incomplete while with no tDo" ~: parseFail "cattimp a" tDo

 , "no tEndIf for simple if" ~: parseFail "daca a atunci" tEndIf

 , "incomplete for" ~: parseFail "pt a=1;a<=2;a=a+1 executa" tDo

 , "for with expression as initial" ~: parseFail "pt a+1;2;a=2 executa sfpt" "unexpected \"+\""

 , "incomplete for (without tDo)" ~: parseFail "pt a=1;a<=2;a=a+1" tDo

 , "tDo as variable" ~: parseFail tDo tDo

 , "tOr as variable" ~: parseFail tOr tOr

 , "tAnd as variable" ~: parseFail tAnd tAnd

 , "run parser, parse error" ~:
    "parse error at" `isPrefixOf` runParser "a(" @? "parser succeeded"
 ]


{---TODO:
error for: epc> pt a=func() sffunc;2;a=3 executa sfpt
[For (Just (Assign (Var "a") (FuncDef "" [] []))) (BinExpr Le (Var "a") (Int 2)) (Assign (Var "a") (E (Int 3))) []]

error for:
epc> pt a=2;a<=2;a=func() sffunc executa sfpt
[For (Just (Assign (Var "a") (E (Int 2)))) (BinExpr Le (Var "a") (Int 2)) (Assign (Var "a") (FuncDef "" [] [])) []]
-}
