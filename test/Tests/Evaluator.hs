module Tests.Evaluator (evaluatorTests)
where

import Data.List (isInfixOf)

import Test.HUnit

import EPseudocode.Data
import EPseudocode.Evaluator
import EPseudocode.Lexer

evalTest :: String -> String
evalTest input = case interpret [] input of
    Left err -> err
    Right res -> showExpr . snd $ res

evalFail input needle = case interpret [] input of
    Left err -> needle `isInfixOf` err @? "eval failed (" ++ err ++ "), but could not find needle: " ++ needle
    Right _ -> False @? "eval succeeded"

evaluatorTests = TestList [
   "int in, int out" ~:
    "1" ~=? evalTest "1"

 , "false in false out" ~:
    tFalse ~=? evalTest tFalse

 , "list comparisons, with vars" ~:
    tTrue ~=? evalTest "a={1,2} b={2,3} a<b"

 , "nested list index access" ~:
    "{1, {{5, 6}, 3}, 4}" ~=? evalTest "a={1, {2, 3}, 4} a[1][0] = {5,6} a"

 , "pow" ~:
    "16.0" ~=? evalTest "2**4"

 , "pow with var" ~:
    "16.0" ~=? evalTest "a=2 a**4"

 , "div" ~:
    "2.0" ~=? evalTest "4/2"

 , "div with var" ~:
    "0.25" ~=? evalTest "a=1 b=4 a/b"

 , "fals and fals" ~: evalFail "fals and fals" "Unbound"

 , "assign fals" ~:
    "fals" ~=? evalTest "a=fals"

 , "non-bool condition" ~: evalFail "daca 1 atunci 1 sfdaca" "Bool"

    --TODO: this shouldn't be possible, fix in parser?
 , "empty simpleif" ~: evalFail "daca adevarat atunci sfdaca" "asd"

 , "mod in if" ~:
    "3" ~=? evalTest "daca 4%2 == 0 atunci 7%4 sfdaca"

--TODO: do I really want ifs to return the last value?
 , "mod in if" ~:
    tFalse ~=? evalTest "daca fals atunci -1 sfdaca"

 ]
