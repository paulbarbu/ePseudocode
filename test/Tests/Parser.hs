{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Parser (parserTests, parseFile)
where

import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T

import Test.HUnit
import Text.RawString.QQ

import EPseudocode.Parser (eParse, runParser, mainParser, toplevelParser)
import EPseudocode.Lexer
import EPseudocode.Data
import EPseudocode.Helpers


trim = T.unpack . T.replace ", " "," . T.pack . unwords . words

parse p input = case eParse p input of
    Left err -> error err
    Right expr -> expr

parseRepl input = show $ parse mainParser input
parseFile input = show $ parse toplevelParser input

parseFail p input needle = case eParse p input of
    Left err -> needle `isInfixOf` err @? "parser succeeded: " ++ err
    Right ast -> error $ "parser succeeded with: " ++ show ast

parseFailRepl input needle = parseFail mainParser input needle
parseFailFile input needle = parseFail toplevelParser input needle

{- TODO: improve error messages:
"daca 2 atunci ret 2 altfel sf" --because the sfdaca is not complete the error si ambigous
"daca a atunci altfel a( sfdaca" -- the error is on the "else" branch but the keyword "altfel" is blamed
"daca 2 atucni sfdaca" -- unexpected "a"
-}

parserTests = TestList [
   "assign list to the variable a" ~:
    trim [r|[Assign (Var "a") (List [Int 1])]|] ~=? parseRepl "a={1}"

 , "assign int to the variable a" ~:
    trim [r|[Assign (Var "a") (Int 1)]|] ~=? parseRepl "a=1"

 , "function call" ~:
    trim [r|[E (FuncCall (Var "a") [[]])]|] ~=? parseRepl "a()"

 , "multiple indexing a list" ~:
    trim [r|[E (Index "a" [Int 1,Int 2,Int 3,Int 4])]|] ~=? parseRepl "a[1][2][3][4]"

 , "function call with function calls as args" ~:
    trim [r|[E (FuncCall (Var "a") [[FuncCall (Var "b") [[]],FuncCall (Var "c") [[]]]])]|] ~=? parseRepl "a(b(), c())"

 , "function call with list index arg" ~:
    trim [r|[E (FuncCall (Var "scrie") [[Index "a" [Int 2]]])]|] ~=? parseRepl "scrie(a[2])"

 , "simple if and assignment" ~:
    trim [r|[SimpleIf (BinExpr Ge (Var "a") (Int 2))
              [Assign (Var "a")  (BinExpr Plus (Var "a") (Int 2))]]|] ~=? parseRepl "daca a>=2 atunci a=a+2 sfdaca"

 , "simple if with index condition" ~:
    trim [r|[SimpleIf (Index "a" [Int 2])
              [Assign (Var "a") (BinExpr Plus (Var "a") (Int 2))]]|] ~=? parseRepl "daca a[2] atunci a=a+2 sfdaca"

 , "simple func def" ~:
    trim [r|[E (FuncDef "x" [] [Ret (Int 3)])]|] ~=? parseRepl "func x() ret 3 sffunc"

 , "assign function (returns function call) to variable" ~:
    trim [r|[Assign (Var "a") (FuncDef "x" [] [Ret (FuncCall (Var "y") [[]])])]|] ~=? parseRepl "a=func x() ret y() sffunc"

 , "list containing lambdas" ~:
    trim [r|[E (List [Int 1,Int 5,FuncDef "" [] [Ret (Int 42)],FuncDef "" [] [Ret (Int 43)]])]|] ~=? parseRepl "{1, 5, func() ret 42 sffunc, func() ret 43 sffunc}"

 , "simple 'for' program" ~:
    do c <- readFile "examples/for.epc"
       trim [r|[E (FuncDef "main" [] [Assign (Var "sum") (Int 0),
        For (Just (Assign (Var "i") (Int 1))) (Just (BinExpr Le (Var "i") (Int 100))) (Just (Assign (Var "i") (BinExpr Plus (Var "i") (Int 1))))
            [Assign (Var "sum") (BinExpr Plus (Var "sum") (Var "i"))],
        E (FuncCall (Var "scrie") [[String "Sum = ",Var "sum",String "\n"]])])]|] @=? parseFile c

 , "simple 'hello world' program" ~:
    do c <- readFile "examples/hello.epc"
       trim [r|[E (FuncDef "main" [] [E (FuncCall (Var "scrie") [[String "Hello world\n"]])])]|] @=? parseFile c

 , "process a range of numbers with a callback function and a custom step" ~:
    do c <- readFile "examples/callback.epc"
       trim [r|[E (FuncDef "applyToRange" ["a","b","step","f"]
                [For (Just (Assign (Var "i") (Var "a"))) (Just (BinExpr Le (Var "i") (Var "b"))) (Just (Assign (Var "i") (FuncCall (Var "step") [[Var "i"]])))
                     [E (FuncCall (Var "f") [[Var "i"]])]]),
        E (FuncDef "main" []
                [E (FuncCall (Var "applyToRange")
                    [[Int 1,Int 5,
                      FuncDef "" ["x"]
                              [Ret (BinExpr Plus (Var "x") (Int 1))],
                      FuncDef "" ["x"]
                              [E (FuncCall (Var "scrie") [[BinExpr Mul (Var "x") (Int 2)]])]]])])]|] @=? parseFile c

 , "compute the n-th fibonacci number" ~:
    do c <- readFile "examples/fib.epc"
       trim [r|[E (FuncDef "fib" ["n"]
                [SimpleIf (BinExpr Le (Var "n") (Int 2))
                          [Ret (Int 1)],
                Ret (BinExpr Plus (FuncCall (Var "fib") [[BinExpr Minus (Var "n") (Int 1)]])
                                     (FuncCall (Var "fib") [[BinExpr Minus (Var "n") (Int 2)]]))]),
        E (FuncDef "main" [] [E (FuncCall (Var "scrie") [[FuncCall (Var "fib") [[Int 10]],String "\n"]])])]|] @=? parseFile c

 , "display indices in a list along with values" ~:
    do c <- readFile "examples/lists.epc"
       trim [r|[E (FuncDef "main" [] [Assign (Var "l") (List [Int 1,Int 2,Int 5,Int 7,Int 8]),
        For (Just (Assign (Var "i") (Int 0))) (Just (BinExpr Lt (Var "i") (FuncCall (Var "lung") [[Var "l"]]))) (Just (Assign (Var "i") (BinExpr Plus (Var "i") (Int 1))))
            [E (FuncCall (Var "scrie") [[Var "i",String ":",Index "l" [Var "i"],String "\n"]])]])]|] @=? parseFile c

 , "greatest common divisor program" ~:
    do c <- readFile "examples/greatest_common_div.epc"
       trim [r|[E (FuncDef "greatestCommonDivisor" ["a","b"]
                [While (BinExpr Neq (Var "a") (Var "b"))
                       [CompleteIf (BinExpr Gt (Var "a") (Var "b"))
                                   [Assign (Var "a") (BinExpr Minus (Var "a") (Var "b"))]
                                   [Assign (Var "b") (BinExpr Minus (Var "b") (Var "a"))]],
                 Ret (Var "a")]),
        E (FuncDef "gcd" ["a","b"]
                [SimpleIf (BinExpr Eq (Var "b") (Int 0))
                          [Ret (Var "a")],
                 E (FuncCall (Var "gcd") [[Var "b",BinExpr Mod (Var "a") (Var "b")]])]),
        E (FuncDef "main" []
                [E (FuncCall (Var "scrie") [[FuncCall (Var "gcd") [[Int 5,Int 25]],String "\n"]]),
                 E (FuncCall (Var "scrie") [[FuncCall (Var "greatestCommonDivisor") [[Int 3,Int 5]],String "\n"]])])]|] @=? parseFile c

 , "simple closure program" ~:
    do c <- readFile "examples/closure.epc"
       trim [r|[E (FuncDef "plusN" ["n"]
                [Ret (FuncDef "" ["x"]
                              [Ret (BinExpr Plus (Var "n") (Var "x"))])]),
        E (FuncDef "main" []
                [Assign (Var "plus3") (FuncCall (Var "plusN") [[Int 3]]),
                 E (FuncCall (Var "scrie") [[FuncCall (Var "plus3") [[Int 5]]]])])]|] @=? parseFile c

  , "fizzbuzz program" ~:
    do c <- readFile "examples/fizzbuzz.epc"
       trim [r|[E (FuncDef "main" []
        [E (FuncCall (Var "scrie") [[String "n="]]),
        Assign (Var "n") (FuncCall (Var "int") [[FuncCall (Var "citeste") [[]]]]),
        CompleteIf (Index "n" [Int 0]) [Assign (Var "n") (Index "n" [Int 1]),
        Assign (Var "i") (Int 1),
        While (BinExpr Le (Var "i") (Var "n"))
              [CompleteIf (BinExpr And
                                    (BinExpr Eq (BinExpr Mod (Var "i") (Int 3)) (Int 0))
                                    (BinExpr Eq (BinExpr Mod (Var "i") (Int 5)) (Int 0)))
                          [E (FuncCall (Var "scrie") [[String "fizzbuzz\n"]])]
                          [CompleteIf (BinExpr Eq (BinExpr Mod (Var "i") (Int 3)) (Int 0))
                                      [E (FuncCall (Var "scrie") [[String "fizz\n"]])]
                                      [CompleteIf (BinExpr Eq (BinExpr Mod (Var "i") (Int 5)) (Int 0))
                                                  [E (FuncCall (Var "scrie") [[String "buzz\n"]])]
                                                  [E (FuncCall (Var "scrie") [[Var "i",String "\n"]])]]],
               Assign (Var "i") (BinExpr Plus (Var "i") (Int 1))]] [E (FuncCall (Var "scrie") [[String "Invalid n!\n"]])]])]|] @=? parseFile c

  , "global variable program" ~:
     do c <- readFile "examples/global.epc"
        trim [r|[Assign (Var "a")
                (FuncDef "" []
                 [Ret (Int 42)]),
         E (FuncDef "main" []
          [E (FuncCall (Var "scrie") [[FuncCall (Var "a") [[]],String "\n"]])])]|] @=? parseFile c

 , "imbricated ifs" ~:
    trim [r|[SimpleIf (Int 1)
              [SimpleIf (Int 2)
                        [Ret (Int 3)],
               SimpleIf (Int 42)
                        [Ret (Int 5)]]]|] ~=? parseRepl "daca 1 atunci daca 2 atunci ret 3 sfdaca daca 42 atunci ret 5 sfdaca sfdaca"

 , "sequential ifs (complete, simple)" ~:
    trim [r|[CompleteIf (Int 1)
                [Ret (Int 2)]
                [Ret (Int 3)],
     SimpleIf (Int 42)
              [Ret (Int 5)]]|] ~=? parseRepl "daca 1 atunci ret 2 altfel ret 3 sfdaca daca 42 atunci ret 5 sfdaca"

 , "function with sequential simple ifs" ~:
    trim [r|[E (FuncDef "x" []
             [SimpleIf (Int 1)
                       [Ret (Int 2)],
              SimpleIf (Int 3)
                       [Ret (Int 42)]])]|] ~=? parseRepl "func x() daca 1 atunci ret 2 sfdaca daca 3 atunci ret 42 sfdaca sffunc"

 , "lists and function calls passed to each other" ~:
    trim [r|[E (List [Int 1,Int 2,
              FuncCall (Var "a") [[List [Int 3,Int 4,
                                               FuncCall (Var "b") [[]]],
              Int 5]]])]|] ~=? parseRepl "{1, 2, a({3, 4, b()}, 5)}"

 , "function returns function" ~:
    trim [r|[E (FuncDef "a" []
             [Ret (FuncDef "" []
                           [Ret (Int 1)])])]|] ~=? parseRepl "func a() ret func() ret 1 sffunc sffunc"

 , "function call with anon function as arg" ~:
    trim [r|[E (FuncCall (Var "a") [[FuncDef "" []
                                     [Ret (Int 42)]]])]|] ~=? parseRepl "a(func() ret 42 sffunc)"

 , "function with sequential if and return" ~:
    trim [r|[E (FuncDef "x" []
             [Assign (Var "a") (Bool False),
              SimpleIf (Var "a")
                       [Assign (Var "a") (Bool True)],
              Ret (Int 42)])]|] ~=? parseRepl "func x() a=fals daca a atunci a=adevarat sfdaca ret 42 sffunc"

 , "multiple function call - first function returned a function" ~:
    trim [r|[E (FuncCall (Var "a") [[],[]])]|] ~=? parseRepl "a()()"

 , "multiple function call - more general" ~:
    trim [r|[E (FuncCall (Var "a") [[],[],[],[],[],[],[],[]])]|] ~=? parseRepl "a()()()()()()()()"

 , "list index operation returns a function which is in turn called" ~:
    trim [r|[E (FuncCall (Index "a" [Int 1]) [[]])]|] ~=? parseRepl "a[1]()"

 , "list indexed by function call returns function that is in turn called" ~:
    trim [r|[E (FuncCall (Index "a" [FuncCall (Var "foo") [[]]]) [[]])]|] ~=? parseRepl "a[foo()]()"

 , "function call, one of the args being a lambda" ~:
    trim [r|[E (FuncCall (Var "a") [[Int 1,FuncDef "" [] [Ret (Int 2)]]])]|] ~=? parseRepl "a(1, func() ret 2 sffunc)"

 , "function call with other function calls as args" ~:
    trim [r|[E (FuncCall (Var "a") [[FuncCall (Var "b") [[]],FuncCall (Var "c") [[],[Int 1]]]])]|] ~=? parseRepl "a(b(), c()(1))"

 , "complete if with function calls" ~:
    trim [r|[CompleteIf (FuncCall (Var "a") [[FuncCall (Var "b") [[]],FuncCall (Var "c") [[]]]])
                [E (FuncCall (Var "a") [[FuncCall (Var "b") [[]],FuncCall (Var "c") [[]]]])]
                [E (FuncCall (Var "a") [[Int 1,Int 2]])]]|] ~=? parseRepl "daca a(b(), c()) atunci a(b(), c()) altfel a(1, 2) sfdaca"

 , "function followed by if" ~:
    trim [r|[E (FuncDef "x" []
             [SimpleIf (Int 1)
                       [Ret (Int 2)]]),
     SimpleIf (Int 3)
              [Ret (Float 4.2)]]|] ~=? parseRepl "func x() daca 1 atunci ret 2 sfdaca sffunc daca 3 atunci ret 4.2 sfdaca"


 , "return followed by return" ~:
    trim [r|[E (FuncDef "x" []
             [Ret (FuncDef "y" []
                           [Ret (UnExpr UnMinus (Int 1))]),
              Ret (Int 2)])]|] ~=? parseRepl "func x() ret func y() ret -1 sffunc ret 2 sffunc"

 , "return followed by simple if" ~:
    trim [r|[E (FuncDef "x" []
             [Ret (FuncDef "y" []
                           [Ret (Int 1)]),
              SimpleIf (Int 2)
                       [Ret (Int 3)]])]|] ~=? parseRepl "func x() ret func y() ret 1 sffunc daca 2 atunci ret 3 sfdaca sffunc"

 , "simple if with sequence of statements" ~:
    trim [r|[SimpleIf (UnExpr Not (Bool False))
              [Assign (Var "a") (BinExpr Mul (BinExpr Minus (Int 2) (Int 1)) (Int 3)),
               Assign (Var "b") (Int 3),
               Assign (Var "c") (Int 4)]]|] ~=? parseRepl "daca !fals atunci a=(2-1)*3 b=3 c=4 sfdaca"

 , "complete if with sequence of statements" ~:
    trim [r|[CompleteIf (BinExpr Or (Int 1) (Int 1))
                [Assign (Var "a") (Int 2),
                 Assign (Var "b") (Int 3),
                 Assign (Var "c") (Int 4)]
                [Assign (Var "a") (Int 3),
                 Assign (Var "b") (Int 42),
                 Assign (Var "c") (Int 5)]]|] ~=? parseRepl "daca 1 sau 1 atunci a=2 b=3 c=4 altfel a=3 b=42 c=5 sfdaca"

 , "for with sequence of statements" ~:
    trim [r|[For (Just (Assign (Var "a") (Int 1))) (Just (BinExpr Lt (Var "a") (Int 42))) (Just (Assign (Var "a") (BinExpr Plus (Var "a") (Int 1))))
         [Assign (Var "b") (Int 3),
          Assign (Var "c") (Var "a")]]|] ~=? parseRepl "pt a=1;a<42;a=a+1 executa b=3 c=a sfpt"

 , "while with sequence of statements" ~:
    trim [r|[While (Bool True)
           [Assign (Var "a") (Int 1),
            Assign (Var "b") (BinExpr Div (Int 2) (Int 4)),
            Assign (Var "c") (Int 3)]]|] ~=? parseRepl "cattimp adevarat executa a=1 b=2/4 c=3 sfcattimp"

 , "function definition with sequence of statements" ~:
    trim [r|[E (FuncDef "a" []
             [Assign (Var "b") (Int 3),
              Assign (Var "c") (Int 4)])]|] ~=? parseRepl "func a() b=3 c=4 sffunc"

 , "pow(positive int, positive int)" ~:
    trim [r|[E (BinExpr Pow (Int 2) (Int 4))]|] ~=? parseRepl "2 ** 4"

 , "-pow(positive int, positive int)" ~:
    trim [r|[E (UnExpr UnMinus (BinExpr Pow (Int 2) (Int 4)))]|] ~=? parseRepl "-2 ** 4"

 , "pow(negative expr, positive int)" ~:
    trim [r|[E (BinExpr Pow (UnExpr UnMinus (Int 2)) (Int 4))]|] ~=? parseRepl "(-2) ** 4"

 , "pow(negative expr, negative int)" ~:
    trim [r|[E (BinExpr Pow (UnExpr UnMinus (Int 2)) (Int (-4)))]|] ~=? parseRepl "(-2) ** -4"

 , "pow(negative expr, negative int) - no space" ~:
    trim [r|[E (BinExpr Pow (UnExpr UnMinus (Int 2)) (Int (-4)))]|] ~=? parseRepl "(-2) **-4"

 , "pow(negative expr, negative expr)" ~:
    trim [r|[E (BinExpr Pow (UnExpr UnMinus (Int 2)) (UnExpr UnMinus (Int 4)))]|] ~=? parseRepl "(-2) ** (-4)"

 , "pow(int , float)" ~:
    trim [r|[E (BinExpr Pow (Int 4) (Float 2.5))]|] ~=? parseRepl "4 ** 2.5"

 , "expression" ~:
    trim [r|[E (BinExpr Minus (BinExpr Plus (BinExpr Plus (Int 15) (Int 1)) (Int 4)) (Int 5))]|] ~=? parseRepl " 0xF +  1 + /*com/*inside comment*/ment*/   4 - 5 //comment"

 , "empty while" ~:
    trim [r|[While (BinExpr Mod (Int 2) (Int 3)) []]|] ~=? parseRepl "cattimp 2%3 executa sfcattimp"

 , "empty complete if" ~:
    trim [r|[CompleteIf (Int 2) [] []]|] ~=? parseRepl "daca 2 atunci altfel sfdaca"

 , "empty for" ~:
    trim [r|[For (Just (Assign (Var "a") (Int 1)))
         (Just (BinExpr Le (Var "a") (Int 12)))
         (Just (Assign (Var "a") (BinExpr Plus (Var "a") (Int 3)))) []]|] ~=? parseRepl "pt a=1;a<=12;a=a+3 executa sfpt"

 , "infinite empty for" ~:
    trim [r|[For Nothing Nothing Nothing []]|] ~=? parseRepl "pt ;; executa sfpt"

 , "empty simple if" ~:
    trim [r|[SimpleIf (BinExpr Mod (Var "a") (Int 2)) []]|] ~=? parseRepl "daca a%2 atunci sfdaca"

 , "assign to list index" ~:
    trim [r|[Assign (Index "a" [Int 2]) (Int 3)]|] ~=? parseRepl "a[2]=3"

 , "for with list indices" ~:
    trim [r|[For (Just (Assign (Index "a" [Int 2]) (Int 1)))
         (Just (BinExpr Le (Index "a" [Int 2]) (Int 5)))
         (Just (Assign (Index "a" [Int 2]) (Int 3))) []]|] ~=? parseRepl "pt a[2]=1;a[2]<=5;a[2]=3 executa sfpt"

 , "run parser, simple expression" ~:
    "[E (Int 1)]" ~=? runParser mainParser "1"

 , "run parser, simple minus expression" ~:
    "[E (UnExpr UnMinus (Int 1))]" ~=? runParser mainParser "-1"

 , "run parser, simple expression" ~:
    "[E (FuncCall (Var \"a\") [[BinExpr Div (Int 5) (Int 2)]])]" ~=? runParser mainParser "a(5/2)"

 , "incomplete if and function call" ~: parseFailRepl "daca a(" tThen

 , "incomplete function def" ~: parseFailRepl "func a" "parameters list"

 , "incomplete parameter listf" ~: parseFailRepl "func a()" tEndFunc

 , "incomplete while" ~: parseFailRepl "cattimp adevarat executa" tDo

 , "incomplete while with no tDo" ~: parseFailRepl "cattimp a" tDo

 , "no tEndIf for simple if" ~: parseFailRepl "daca a atunci" tEndIf

 , "incomplete for" ~: parseFailRepl "pt a=1;a<=2;a=a+1 executa" tDo

 , "for with expression as initial" ~: parseFailRepl "pt a+1;2;a=2 executa sfpt" "unexpected \"+\""

 , "for with function as initial" ~: parseFailRepl "pt a=func() sffunc;2;a=3 executa sfpt" "unexpected reserved word \"func\""

 , "for with function as initial" ~: parseFailRepl "pt a=2;a<=2;a=func() sffunc executa sfpt" "unexpected reserved word \"func\""

 , "incomplete for (without tDo)" ~: parseFailRepl "pt a=1;a<=2;a=a+1" tDo

 , "tDo as variable" ~: parseFailRepl tDo tDo

 , "tOr as variable" ~: parseFailRepl tOr tOr

 , "tAnd as variable" ~: parseFailRepl tAnd tAnd

 , "run parser, parseRepl error" ~:
    "parse error at" `isPrefixOf` runParser mainParser "a(" @? "parser succeeded"

 , "empty repl line" ~: parseFailRepl "" "unexpected end of input"

 , "empty file" ~: parseFailFile "" "unexpected end of input"

 , "function call in global scope" ~: parseFailFile "func main() ret 1 sffunc main()" "unexpected \"(\""

 , "global definitions" ~:
    do c <- readFile "examples/global.epc"
       trim [r|[Assign (Var "a") (FuncDef "" []
                                  [Ret (Int 42)]),
        E (FuncDef "main" []
                [E (FuncCall (Var "scrie") [[FuncCall (Var "a") [[]],String "\n"]])])]|] @=? parseFile c

 , "index a list with another list index" ~:
    trim [r|[E (Index "a" [Index "b" [Int 1]])]|] ~=? parseRepl "a[b[1]]"
 ]
