module Tests.Data (dataTests)
where

import Test.HUnit

import EPseudocode.Lexer
import EPseudocode.Data


dataTests = TestList [
   "list" ~:
    "{1, 2}" ~=? showExpr (List [Int 1, Int 2])

 , "nested list" ~:
    "{1, {4, 5}, 3}" ~=? showExpr (List [Int 1, List [Int 4, Int 5], Int 3])
 , "float" ~:
    "42.42" ~=? showExpr (Float 42.42)
 , "string" ~:
    "\"foobar\"" ~=? showExpr (String "foobar")
 , "bool true" ~:
    tTrue ~=? showExpr (Bool True)
 , "bool false" ~:
    tFalse ~=? showExpr (Bool False)
 , "unary expr unminus" ~:
    "UnExpr UnMinus (Int 42)" ~=? showExpr (UnExpr UnMinus (Int 42))
 , "unary expr not" ~:
    "UnExpr Not (Bool True)" ~=? showExpr (UnExpr Not (Bool True))
 ]
