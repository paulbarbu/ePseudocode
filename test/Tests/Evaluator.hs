module Tests.Evaluator (evaluatorTests)
where

import Data.List (isInfixOf)

import Test.HUnit

import EPseudocode.Data
import EPseudocode.Evaluator
import EPseudocode.Lexer
import EPseudocode.Parser

evalTest :: String -> String
evalTest input = case eParse mainParser input >>= mapM (eval []) of
    Left err -> err
    Right res -> unwords (map (showExpr . snd) res)

evalFail input needle = case eParse mainParser input >>= mapM (eval []) of
    Left err -> needle `isInfixOf` err @? "eval failed (" ++ err ++ "), but could not find needle: " ++ needle
    Right _ -> False @? "eval succeeded"

evaluatorTests = TestList [
   "int in, int out" ~:
    "1" ~=? evalTest "1"

 , "false in folse out" ~:
    tFalse ~=? evalTest tFalse

-- TODO: do not eval these individually"
 , "list comparisons, with vars" ~:
    tFalse ~=? evalTest "a={1,2} b={2,3} a<b"

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
 ]
