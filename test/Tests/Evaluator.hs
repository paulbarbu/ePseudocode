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

 , "nested list 2nd level index modification" ~:
    "{1, {{5, 6}, 3}, 4}" ~=? evalTest "a={1, {2, 3}, 4} a[1][0] = {5,6} a"

 , "invalid nested list index access" ~: evalFail "a={1, {{5, 6}, 3}, 4} a[1][-1]" "Invalid nested list index: -1"

 , "invalid nested list index access" ~: evalFail "a={1, {{5, 6}, 3}, 4} a[-1]" "Invalid list index: a[-1]"

 , "pow" ~:
    "16.0" ~=? evalTest "2**4"

 , "pow with var" ~:
    "16.0" ~=? evalTest "a=2 a**4"

 , "div" ~:
    "2.0" ~=? evalTest "4/2"

 , "div with var" ~:
    "0.25" ~=? evalTest "a=1 b=4 a/b"

 , "fals and fals, unbound \"and\"" ~: evalFail "fals and fals" "Unbound"

 , "assign fals" ~:
    "fals" ~=? evalTest "a=fals"

 , "non-bool condition" ~: evalFail "daca 1 atunci 1 sfdaca" "Bool"

 , "empty simple if" ~:
    "" ~=? evalTest "daca adevarat atunci sfdaca"

 , "mod in simple if, no return value" ~:
    "" ~=? evalTest "daca 4%2 == 0 atunci 7%4 sfdaca"

 , "false simple if condition" ~:
    "" ~=? evalTest "daca fals atunci -1 sfdaca"

 , "true in complete if condition" ~:
    "" ~=? evalTest "daca adevarat atunci 42 altfel 52 sfdaca"

 , "false in complete if condition" ~:
    "" ~=? evalTest "daca fals atunci 42 altfel 52 sfdaca"

 , "false in complete if condition, empty branch" ~:
    "" ~=? evalTest "daca fals atunci 42 altfel sfdaca"

 , "true in complete if condition, empty branch" ~:
    "" ~=? evalTest "daca fals atunci altfel 52 sfdaca"

 , "2**10 in while" ~:
    "adevarat" ~=? evalTest "b={10, 1} cattimp b[0] > 0 executa b[0]=b[0]-1 b[1]=b[1]*2 sfcattimp 2**10 == b[1] si 0 == b[0]"

 , "false while condition" ~:
   "" ~=? evalTest "cattimp fals executa sfcattimp"

 , "non-bool while condition" ~: evalFail "cattimp 42 executa sfcattimp" "A loop's condition should evaluate to Bool"

 , "unbound index access" ~: evalFail "a[1]" "Unbound variable name"

 , "unbound nested index access" ~: evalFail "a[0][1]" "Unbound variable name"

 , "non-integer index access" ~: evalFail "a={1,2} a[adevarat]" "List can be indexed only with Integer evaluating expressions"

 , "non-integer nested index access" ~: evalFail "a={{1,2}} a[0][4.2]" "List can be indexed only with Integer evaluating expressions"

 , "non-integer 2nd level nested index access" ~: evalFail "a={{1,{2}}} a[0][1][4.2]" "List can be indexed only with Integer evaluating expressions"

 , "invalid 2nd level nested index access" ~: evalFail "a={{1,{2}}} a[0][1][1]" "Invalid nested list index"

 , "non-list 2nd level nested index access" ~: evalFail "a={{1,{2}}} a[0][1][0][0]" "Only Lists and Strings can be indexed"

 , "non-list nested index access" ~: evalFail "a={1} a[0][1]" "Only Lists and Strings can be indexed"

 , "invalid nested index access" ~: evalFail "a={{1}} a[5][-1]" "Invalid list index"

 , "invalid 2nd level nested index access" ~: evalFail "a={{1}} a[0][-1][0]" "Invalid nested list index"

 , "non-integer 2nd level nested index access" ~: evalFail "a={{1}} a[0][4.2][0]" "List can be indexed only with Integer evaluating expressions"

 , "non-integer nested index access, but the error is at first index" ~: evalFail "a={1,2} a[0.1][4]" "List can be indexed only with Integer evaluating expressions"

 , "nested list 3rd level index modification" ~:
    "{1, {2, {{5, 6}, 5}}, 4}" ~=? evalTest "a={1, {2, {3, 5}}, 4} a[1][1][0] = {5,6} a"

 , "nested list 3rd level index modification" ~:
    "3" ~=? evalTest "a={1, {2, 3}, 4} a[1][1]"

 , "string" ~:
    "abc" ~=? evalTest "a=\"foobar\" \"abc\""

 , "not false == true" ~:
    "adevarat" ~=? evalTest "!fals == adevarat"

 , "minus true == false" ~:
    "adevarat" ~=? evalTest "-adevarat == fals"

 , "-42" ~:
    "-42" ~=? evalTest "-42"

 , "-(1-42)" ~:
    "41.2" ~=? evalTest "-(1-42.2)"

 , "-4.2" ~:
    "-4.2" ~=? evalTest "-4.2"

 , "string reverse" ~:
    "cba" ~=? evalTest "-\"abc\""

 , "list reverse" ~:
    "{3, 2, 1}" ~=? evalTest "-{1, 2, 3}"

 , "not integer error" ~: evalFail "!42" "Integer cannot be negated"

 , "not float error" ~: evalFail "!42.42" "Float cannot be negated"

 , "not list error" ~: evalFail "!{1, 2}" "List cannot be negated"

 , "not string error" ~: evalFail "!\"abc\"" "String cannot be negated"

 , "error in simple if cond" ~: evalFail "daca {}>2 atunci 1 sfdaca" "Lists can be compared only to lists"

 , "error in complete if cond" ~: evalFail "daca {}>2 atunci 1 altfel 2 sfdaca" "Lists can be compared only to lists"

 , "non-bool cond in complete if" ~: evalFail "daca {} atunci 1 altfel 2 sfdaca" "An If's condition should evaluate to Bool"

 , "{} si 1" ~: evalFail "{} si 1" "Cannot apply and to a list"

 , "1 si {}" ~: evalFail "1 si {}" "Cannot apply and to a list"

 , "1 sau {}" ~: evalFail "1 sau {}" "Cannot apply or to a list"

 , "{} sau 1" ~: evalFail "{} sau 1" "Cannot apply or to a list"

 , "1 * {}" ~: evalFail "1 * {}" "Cannot multiply a list"

 , "{} * 1" ~: evalFail "{} * 1" "Cannot multiply to a list"

 , "1 / {}" ~: evalFail "1 / {}" "Cannot divide to a list"

 , "{} / 1" ~: evalFail "{} / 1" "Cannot divide a list"

 , "1 % {}" ~: evalFail "1 % {}" "Cannot apply mod to a list"

 , "{} % 1" ~: evalFail "{} % 1" "Cannot apply mod to a list"

 , "1 ** {}" ~: evalFail "1 ** {}" "Cannot raise to a list power"

 , "{} ** 1" ~: evalFail "{} ** 1" "Cannot raise a list to power"

 , "{} < 1" ~: evalFail "{} < 1" "Lists can be compared only to lists"

 , "1 < {}" ~: evalFail "1 < {}" "Lists can be compared only to lists"

 , "{} <= 1" ~: evalFail "{} <= 1" "Lists can be compared only to lists"

 , "1 <= {}" ~: evalFail "1 <= {}" "Lists can be compared only to lists"

 , "{} > 1" ~: evalFail "{} > 1" "Lists can be compared only to lists"

 , "1 > {}" ~: evalFail "1 > {}" "Lists can be compared only to lists"

 , "{} >= 1" ~: evalFail "{} >= 1" "Lists can be compared only to lists"

 , "1 >= {}" ~: evalFail "1 >= {}" "Lists can be compared only to lists"

 , "{} == 1" ~: evalFail "{} == 1" "Lists can be compared only to lists"

 , "1 == {}" ~: evalFail "1 == {}" "Lists can be compared only to lists"

 , "{} != 1" ~: evalFail "{} != 1" "Lists can be compared only to lists"

 , "1 != {}" ~: evalFail "1 != {}" "Lists can be compared only to lists"

 , "1 - {}" ~: evalFail "1 - {}" "Cannot subtract a list from a value"

 , "1 si 1" ~: evalFail "1 si 1" "And is invalid on Ints"

 , "1 si 1.2" ~: evalFail "1 si 1.2" "And is invalid on Int and Float"

 , "1.2 si 1" ~: evalFail "1.2 si 1" "And is invalid on Float and Int"

 , "\"abc\" si 1" ~: evalFail "\"abc\" si 1" "And is invalid on String and Int"

 , "1 si \"abc\"" ~: evalFail "1 si \"abc\"" "And is invalid on Int and String"

 , "fals si 1" ~: evalFail "fals si 1" "And is invalid on Bool and Int"

 , "1 si fals" ~: evalFail "1 si fals" "And is invalid on Int and Bool"

 , "1.1 si 1.1" ~: evalFail "1.1 si 1.1" "And is invalid on Floats"

 , "\"abc\" si 1.1" ~: evalFail "\"abc\" si 1.1" "And is invalid on String and Float"

 , "1.1 si \"abc\"" ~: evalFail "1.1 si \"abc\"" "And is invalid on Float and String"

 , "fals si 11.1" ~: evalFail "fals si 11.1" "And is invalid on Bool and Float"

 , "1.1 si fals" ~: evalFail "1.1 si fals" "And is invalid on Float and Bool"

 , "\"abc\" si \"abc\"" ~: evalFail "\"abc\" si \"abc\"" "And is invalid on Strings"

 , "fals si \"abc\"" ~: evalFail "fals si \"abc\"" "And is invalid on Bool and String"

 , "\"abc\" si fals" ~: evalFail "\"abc\" si fals" "And is invalid on String and Bool"
 ]
