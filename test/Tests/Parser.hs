module Tests.Parser (parserTests)
where

import Test.HUnit

import EPseudocode.Parser (eParse)
import EPseudocode.Data

--TODO: test the example files

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
    [Assign "a" (FuncDef "x" [] [Ret (E (FuncCall (Var "x") [[]]))])] ~=? parse "a=func x() ret x() sffunc"

 , "list containing lambda" ~:
    [E (List [E (Int 1),E (Int 5),FuncDef "" [] [Ret (E (Int 4))]])] ~=? parse "{1, 5, func() ret 4 sffunc}"

 ]
