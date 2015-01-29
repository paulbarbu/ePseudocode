module Tests.Evaluator (evaluatorTests)
where

import Control.Monad.Except
import Data.List (isInfixOf)
import System.IO

import Test.HUnit

import EPseudocode.Builtins
import EPseudocode.Data
import EPseudocode.Evaluator
import EPseudocode.Lexer
import EPseudocode.Parser

evalTest :: String -> IO String
evalTest input = do
    x <- runExceptT $ interpret builtinEnv input
    case x of
        Left err -> return err
        Right res -> return . showExpr . snd $ res


evalFail input needle = do
    x <- runExceptT $ interpret builtinEnv input
    case x of
        Left err -> if needle `isInfixOf` err then return True else
                        putStrLn ("\nEval failed (" ++ err ++
                            "), but could not find needle: " ++ needle) >>
                             hFlush stdout >> return False
        Right _ -> return False


evalProgram :: String -> [String] -> IO String
evalProgram input argv = case eParse toplevelParser input  of
    Left err -> return $ "failed: " ++ err
    Right p -> do
        res <- runExceptT $ interpretProgram builtinEnv p argv
        case res of
            Left err -> return $ "Error: " ++ err
            Right _ -> return "OK"


evaluatorTests = TestList [
   "int in, int out" ~: do
       r <- evalTest "1"
       "1" @=? r

 , "false in false out" ~: do
    r <- evalTest tFalse
    tFalse @=? r

 , "lists lt, with vars" ~: do
    r <- evalTest "a={1,2} b={2,3} a<b"
    tTrue @=? r

 , "lists le, with vars" ~: do
    r <- evalTest "a={1,2} b={2,3} a<=b"
    tTrue @=? r

 , "lists gt, with vars" ~: do
    r <- evalTest "a={5,4} b={2,3} a>b"
    tTrue @=? r

 , "lists ge, with vars" ~: do
    r <- evalTest "a={5,4} b={2,3} a>=b"
    tTrue @=? r

 , "lists neq, with vars" ~: do
    r <- evalTest "a={5,4} b={2,3} a!=b"
    tTrue @=? r

 , "lists eq, with vars" ~: do
    r <- evalTest "a={2,3} b={2,3} a==b"
    tTrue @=? r

 , "nested list 2nd level index modification" ~: do
    r <- evalTest "b=0 a={1, {2, 3}, 4} a[b+1][b] = {5,6} a"
    "{1, {{5, 6}, 3}, 4}" @=? r

 , "invalid nested list index access" ~: do
    r <- evalFail "a={1, {{5, 6}, 3}, 4} a[1][-1]" "Invalid nested list index: -1"
    True @=? r

 , "invalid nested list index access" ~: do
    r <- evalFail "a={1, {{5, 6}, 3}, 4} a[-1]" "Invalid list index: a[-1]"
    True @=? r

 , "pow" ~: do
    r <- evalTest "2**4"
    "16.0" @=? r

 , "pow with var" ~: do
    r <- evalTest "a=2 a**4"
    "16.0" @=? r

 , "div" ~: do
    r <- evalTest "4/2"
    "2.0" @=? r

 , "div with var" ~: do
    r <- evalTest "a=1 b=4 a/b"
    "0.25" @=? r

 , "fals and fals, unbound \"and\"" ~: do
    r <- evalFail "fals and fals" "Unbound variable name and"
    True @=? r

 , "assign fals" ~: do
    r <- evalTest "a=fals"
    "fals" @=? r

 , "non-bool condition" ~: do
    r <- evalFail "daca 1 atunci 1 sfdaca" "Bool"
    True @=? r

 , "empty simple if" ~: do
    r <- evalTest "daca adevarat atunci sfdaca"
    "" @=? r

 , "mod in simple if, no return value" ~: do
    r <- evalTest "daca 4%2 == 0 atunci 7%4 sfdaca"
    "" @=? r

 , "false simple if condition" ~: do
    r <- evalTest "daca fals atunci -1 sfdaca"
    "" @=? r

 , "true in complete if condition" ~: do
    r <- evalTest "daca adevarat atunci 42 altfel 52 sfdaca"
    "" @=? r

 , "false in complete if condition" ~: do
    r <- evalTest "daca fals atunci 42 altfel 52 sfdaca"
    "" @=? r

 , "false in complete if condition, empty branch" ~: do
    r <- evalTest "daca fals atunci 42 altfel sfdaca"
    "" @=? r

 , "true in complete if condition, empty branch" ~: do
    r <- evalTest "daca fals atunci altfel 52 sfdaca"
    "" @=? r

 , "2**10 in while" ~: do
    r <- evalTest "a=0 b={10, 1} cattimp b[a] > 0 executa b[0]=b[0]-1 b[1]=b[1]*2 sfcattimp 2**10 == b[1] si 0 == b[0]"
    "adevarat" @=? r

 , "false while condition" ~: do
    r <- evalTest "cattimp fals executa sfcattimp"
    "" @=? r

 , "non-bool while condition" ~: do
    r <- evalFail "cattimp 42 executa sfcattimp" "A loop's condition should evaluate to Bool"
    True @=? r

 , "unbound index access" ~: do
    r <- evalFail "a[1]" "Unbound variable name a"
    True @=? r

 , "unbound nested index access" ~: do
    r <- evalFail "a[0][1]" "Unbound variable name a"
    True @=? r

 , "non-integer index access" ~: do
    r <- evalFail "a={1,2} a[adevarat]" "List can be indexed only with Integer evaluating expressions"
    True @=? r

 , "non-integer nested index access" ~: do
    r <- evalFail "a={{1,2}} a[0][4.2]" "List can be indexed only with Integer evaluating expressions"
    True @=? r

 , "non-integer 2nd level nested index access" ~: do
    r <- evalFail "a={{1,{2}}} a[0][1][4.2]" "List can be indexed only with Integer evaluating expressions"
    True @=? r

 , "invalid 2nd level nested index access" ~: do
    r <- evalFail "a={{1,{2}}} a[0][1][1]" "Invalid nested list index: 1"
    True @=? r

 , "non-list 2nd level nested index access" ~: do
    r <- evalFail "a={{1,{2}}} a[0][1][0][0]" "Only Lists and Strings can be indexed"
    True @=? r

 , "non-list nested index access" ~: do
    r <- evalFail "a={1} a[0][1]" "Only Lists and Strings can be indexed"
    True @=? r

 , "invalid nested index access" ~: do
    r <- evalFail "a={{1}} a[5][-1]" "Invalid list index: a[5]"
    True @=? r

 , "invalid 2nd level nested index access" ~: do
    r <- evalFail "a={{1}} a[0][-1][0]" "Invalid nested list index: -1"
    True @=? r

 , "non-integer 2nd level nested index access" ~: do
    r <- evalFail "a={{1}} a[0][4.2][0]" "List can be indexed only with Integer evaluating expressions"
    True @=? r

 , "non-integer nested index access, but the error is at first index" ~: do
    r <- evalFail "a={1,2} a[0.1][4]" "List can be indexed only with Integer evaluating expressions"
    True @=? r

 , "nested list 3rd level index modification" ~: do
    r <- evalTest "b=1 a={1, {2, {3, 5}}, 4} a[1][b][0] = {5,6} a"
    "{1, {2, {{5, 6}, 5}}, 4}" @=? r

 , "nested list 2nd level index access with variable" ~: do
    r <- evalTest "b=1 a={1, {2, {3, 5}}, 4} a[1][b][0]"
    "3" @=? r

 , "nested list 3rd level index access with variables" ~: do
    r <- evalTest "b=0 a={1, {{3, 5}}, 4} a[1][b][b]"
    "3" @=? r

 , "nested list 2nd level index access" ~: do
    r <- evalTest "a={1, {2, 3}, 4} a[1][1]"
    "3" @=? r

 , "string" ~: do
    r <- evalTest "a=\"foobar\" \"abc\""
    "\"abc\"" @=? r

 , "not false == true" ~: do
    r <- evalTest "!fals == adevarat"
    "adevarat" @=? r

 , "minus true == false" ~: do
    r <- evalTest "-adevarat == fals"
    "adevarat" @=? r

 , "-42" ~: do
    r <- evalTest "-42"
    "-42" @=? r

 , "-(1-42)" ~: do
    r <- evalTest "-(1-42.2)"
    "41.2" @=? r

 , "-4.2" ~: do
    r <- evalTest "-4.2"
    "-4.2" @=? r

 , "string reverse" ~: do
    r <- evalTest "-\"abc\""
    "\"cba\"" @=? r

 , "list reverse" ~: do
    r <- evalTest "-{1, 2, 3}"
    "{3, 2, 1}" @=? r

 , "not integer error" ~: do
    r <- evalFail "!42" "Integer cannot be negated"
    True @=? r

 , "not float error" ~: do
    r <- evalFail "!42.42" "Float cannot be negated"
    True @=? r

 , "not list error" ~: do
    r <- evalFail "!{1, 2}" "List cannot be negated"
    True @=? r

 , "not string error" ~: do
    r <- evalFail "!\"abc\"" "String cannot be negated"
    True @=? r

 , "error in simple if cond" ~: do
    r <- evalFail "daca {}>2 atunci 1 sfdaca" "Lists can be compared only to lists"
    True @=? r

 , "error in complete if cond" ~: do
    r <- evalFail "daca {}>2 atunci 1 altfel 2 sfdaca" "Lists can be compared only to lists"
    True @=? r

 , "non-bool cond in complete if" ~: do
    r <- evalFail "daca {} atunci 1 altfel 2 sfdaca" "An If's condition should evaluate to Bool"
    True @=? r

 , "{} si 1" ~: do
    r <- evalFail "{} si 1" "Cannot apply and to a list"
    True @=? r

 , "1 si {}" ~: do
    r <- evalFail "1 si {}" "Cannot apply and to a list"
    True @=? r

 , "1 sau {}" ~: do
    r <- evalFail "1 sau {}" "Cannot apply or to a list"
    True @=? r

 , "{} sau 1" ~: do
    r <- evalFail "{} sau 1" "Cannot apply or to a list"
    True @=? r

 , "1 * {}" ~: do
    r <- evalFail "1 * {}" "Cannot multiply a list"
    True @=? r

 , "{} * 1" ~: do
    r <- evalFail "{} * 1" "Cannot multiply to a list"
    True @=? r

 , "1 / {}" ~: do
    r <- evalFail "1 / {}" "Cannot divide to a list"
    True @=? r

 , "{} / 1" ~: do
    r <- evalFail "{} / 1" "Cannot divide a list"
    True @=? r

 , "1 % {}" ~: do
    r <- evalFail "1 % {}" "Cannot apply mod to a list"
    True @=? r

 , "{} % 1" ~: do
    r <- evalFail "{} % 1" "Cannot apply mod to a list"
    True @=? r

 , "1 ** {}" ~: do
    r <- evalFail "1 ** {}" "Cannot raise to a list power"
    True @=? r

 , "{} ** 1" ~: do
    r <- evalFail "{} ** 1" "Cannot raise a list to power"
    True @=? r

 , "{} < 1" ~: do
    r <- evalFail "{} < 1" "Lists can be compared only to lists"
    True @=? r

 , "1 < {}" ~: do
    r <- evalFail "1 < {}" "Lists can be compared only to lists"
    True @=? r

 , "{} <= 1" ~: do
    r <- evalFail "{} <= 1" "Lists can be compared only to lists"
    True @=? r

 , "1 <= {}" ~: do
    r <- evalFail "1 <= {}" "Lists can be compared only to lists"
    True @=? r

 , "{} > 1" ~: do
    r <- evalFail "{} > 1" "Lists can be compared only to lists"
    True @=? r

 , "1 > {}" ~: do
    r <- evalFail "1 > {}" "Lists can be compared only to lists"
    True @=? r

 , "{} >= 1" ~: do
    r <- evalFail "{} >= 1" "Lists can be compared only to lists"
    True @=? r

 , "1 >= {}" ~: do
    r <- evalFail "1 >= {}" "Lists can be compared only to lists"
    True @=? r

 , "{} == 1" ~: do
    r <- evalFail "{} == 1" "Lists can be compared only to lists"
    True @=? r

 , "1 == {}" ~: do
    r <- evalFail "1 == {}" "Lists can be compared only to lists"
    True @=? r

 , "{} != 1" ~: do
    r <- evalFail "{} != 1" "Lists can be compared only to lists"
    True @=? r

 , "1 != {}" ~: do
    r <- evalFail "1 != {}" "Lists can be compared only to lists"
    True @=? r

 , "1 - {}" ~: do
    r <- evalFail "1 - {}" "Cannot subtract a list from a value"
    True @=? r

 , "1 si 1" ~: do
    r <- evalFail "1 si 1" "And is invalid on Ints"
    True @=? r

 , "1 si 1.2" ~: do
    r <- evalFail "1 si 1.2" "And is invalid on Int and Float"
    True @=? r

 , "1.2 si 1" ~: do
    r <- evalFail "1.2 si 1" "And is invalid on Float and Int"
    True @=? r

 , "\"abc\" si 1" ~: do
    r <- evalFail "\"abc\" si 1" "And is invalid on String and Int"
    True @=? r

 , "1 si \"abc\"" ~: do
    r <- evalFail "1 si \"abc\"" "And is invalid on Int and String"
    True @=? r

 , "fals si 1" ~: do
    r <- evalFail "fals si 1" "And is invalid on Bool and Int"
    True @=? r

 , "1 si fals" ~: do
    r <- evalFail "1 si fals" "And is invalid on Int and Bool"
    True @=? r

 , "1.1 si 1.1" ~: do
    r <- evalFail "1.1 si 1.1" "And is invalid on Floats"
    True @=? r

 , "\"abc\" si 1.1" ~: do
    r <- evalFail "\"abc\" si 1.1" "And is invalid on String and Float"
    True @=? r

 , "1.1 si \"abc\"" ~: do
    r <- evalFail "1.1 si \"abc\"" "And is invalid on Float and String"
    True @=? r

 , "fals si 11.1" ~: do
    r <- evalFail "fals si 11.1" "And is invalid on Bool and Float"
    True @=? r

 , "1.1 si fals" ~: do
    r <- evalFail "1.1 si fals" "And is invalid on Float and Bool"
    True @=? r

 , "\"abc\" si \"abc\"" ~: do
    r <- evalFail "\"abc\" si \"abc\"" "And is invalid on Strings"
    True @=? r

 , "fals si \"abc\"" ~: do
    r <- evalFail "fals si \"abc\"" "And is invalid on Bool and String"
    True @=? r

 , "\"abc\" si fals" ~: do
    r <- evalFail "\"abc\" si fals" "And is invalid on String and Bool"
    True @=? r

 , "for 1;1;1" ~: do
    r <- evalTest "b=-1 pt a=b+1;a<5;a=a+1 executa sfpt a"
    "5" @=? r

 , "for 0;1;1" ~: do
    r <- evalTest "a=0 pt ;a<5;a=a+1 executa sfpt a"
    "5" @=? r

 , "for 0;1;0" ~: do
    r <- evalTest "a=0 pt ;a<5; executa a=a+1 sfpt a"
    "5" @=? r

 , "for 1;1;0" ~: do
    r <- evalTest "pt a=0;a<5; executa a=a+1 sfpt a"
    "5" @=? r

 , "for error condition" ~: do
    r <- evalFail "pt a=0;a<adevarat; executa a=a+1 sfpt a" "Cannot compare Int and Bool"
    True @=? r

 , "lists lt" ~: do
    r <- evalTest "a=1 b=2 {{a,2},4} < {{b,3},5}"
    "adevarat" @=? r

 , "lists le" ~: do
    r <- evalTest "a=1 b=2 {a,b, 42} <= {b,4}"
    "adevarat" @=? r

 , "lists gt" ~: do
    r <- evalTest "a=1 b=2 {a,3} > {b,4}"
    "fals" @=? r

 , "lists ge" ~: do
    r <- evalTest "a=7 b=2 {a,5} >= {b,4}"
    "adevarat" @=? r

 , "lists eq" ~: do
    r <- evalTest "a=7 b=2 {a,2} == {b,4}"
    "fals" @=? r

 , "lists neq" ~: do
    r <- evalTest "a=7 b=3 {a,2} != {b,4}"
    "adevarat" @=? r

 , "remove all elements from list, with variables" ~: do
    r <- evalTest "a=7 b=3 {a, a, 3, b, 7} - a"
    "{3, 3}" @=? r

 , "remove all elements from list, no variables" ~: do
    r <- evalTest "{3,3,3,7,3,7}-3"
    "{7, 7}" @=? r

 , "simple if with env" ~: do
    r <- evalTest "a=42.2 daca a==a atunci a=a-1 sfdaca a"
    "41.2" @=? r

 , "false simple if with env" ~: do
    r <- evalTest "a=42 daca a!=a atunci a=a-1 sfdaca a"
    "42" @=? r

 , "complete if with env" ~: do
    r <- evalTest "a=42 daca a!=a atunci a=a-1 altfel a=a+1 sfdaca a"
    "43" @=? r

 , "float with env" ~: do
    r <- evalTest "a=43.2 b=-(-a)"
    "43.2" @=? r

 , "list append" ~: do
    r <- evalTest "a=4 b=2 {1,b} + {3,a}"
    "{1, 2, {3, 4}}" @=? r

 , "list (variable) append" ~: do
    r <- evalTest "a={1,2} a+{3,4}"
    "{1, 2, {3, 4}}" @=? r

 , "list (variable) append on the right" ~: do
    r <- evalTest "a={1,2} {3,4}+a"
    "{3, 4, {1, 2}}" @=? r

 , "item prepend to list" ~: do
    r <- evalTest "a=1.1 a + {2, 3,4}"
    "{1.1, 2, 3, 4}" @=? r

 , "item append to list" ~: do
    r <- evalTest "b=\"4\" {1,2,3} + b"
    "{1, 2, 3, \"4\"}" @=? r

 , "int ** float" ~: do
    r <- evalTest "3**4.2 < 101 si 3**4.2 > 100"
    "adevarat" @=? r

 , "float ** int" ~: do
    r <- evalTest "4.2**3 < 75 si 4.2**3 > 74"
    "adevarat" @=? r

 , "str ** int" ~: do
    r <- evalFail "\"abc\" ** 3" "Cannot raise String and Int"
    True @=? r

 , "int ** str" ~: do
    r <- evalFail "3 ** \"abc\"" "Cannot raise Int and String"
    True @=? r

 , "bool ** int" ~: do
    r <- evalFail "fals ** 3" "Cannot raise Bool and Int"
    True @=? r

 , "int ** bool" ~: do
    r <- evalFail "3 ** fals" "Cannot raise Int and Bool"
    True @=? r

 , "float ** float" ~: do
    r <- evalTest "3.3**3.3 > 51 si 3.3**3.3 < 52"
    "adevarat" @=? r

 , "str ** float" ~: do
    r <- evalFail "\"abc\" ** 4.3" "Cannot raise String and Float"
    True @=? r

 , "float ** str" ~: do
    r <- evalFail "4.3 ** \"abc\"" "Cannot raise Float and String"
    True @=? r

 , "bool ** float" ~: do
    r <- evalFail "fals ** 4.3" "Cannot raise Bool and Float"
    True @=? r

 , "float ** bool" ~: do
    r <- evalFail "4.3 ** fals" "Cannot raise Float and Bool"
    True @=? r

 , "str ** str" ~: do
    r <- evalFail "\"foo\" ** \"abc\"" "Cannot raise Strings"
    True @=? r

 , "bool ** str" ~: do
    r <- evalFail "fals ** \"abc\"" "Cannot raise Bool and String"
    True @=? r

 , "str ** bool" ~: do
    r <- evalFail "\"abc\" ** fals" "Cannot raise String and Bool"
    True @=? r

 , "bool ** bool" ~: do
    r <- evalFail "fals ** adevarat" "Cannot raise Bools"
    True @=? r

 , "int == float" ~: do
    r <- evalTest "1 == 1.0"
    "adevarat" @=? r

 , "str == str" ~: do
    r <- evalTest "\"abc\" == \"abc\""
    "adevarat" @=? r

 , "str == int" ~: do
    r <- evalFail "\"abc\" == 4" "Cannot compare String and Int"
    True @=? r

 , "int == str" ~: do
    r <- evalFail "4 == \"abc\"" "Cannot compare Int and String"
    True @=? r

 , "bool == int" ~: do
    r <- evalTest "fals == 4"
    "fals" @=? r

 , "int == bool" ~: do
    r <- evalTest "4 == fals"
    "fals" @=? r

 , "str == float" ~: do
    r <- evalFail "\"abc\" == 4.2" "Cannot compare String and Float"
    True @=? r

 , "float == str" ~: do
    r <- evalFail "4.2 == \"abc\"" "Cannot compare Float and String"
    True @=? r

 , "bool == float" ~: do
    r <- evalTest "fals == 4.2"
    "fals" @=? r

 , "float == bool" ~: do
    r <- evalTest "4.2 == fals"
    "fals" @=? r

 , "bool == str" ~: do
    r <- evalTest "fals == \"abc\""
    "fals" @=? r

 , "str == bool" ~: do
    r <- evalTest "\"abc\" == fals"
    "fals" @=? r

 , "int != float" ~: do
    r <- evalTest "1 != 1.2"
    "adevarat" @=? r

 , "float != int" ~: do
    r <- evalTest "1.2 != 1"
    "adevarat" @=? r

 , "float != float" ~: do
    r <- evalTest "1.5 != 1.2"
    "adevarat" @=? r

 , "str != str" ~: do
    r <- evalTest "\"foo\" != \"foo\""
    "fals" @=? r

 , "bool != bool" ~: do
    r <- evalTest "fals != adevarat"
    "adevarat" @=? r

 , "str != int" ~: do
    r <- evalFail "\"abc\" != 4" "Cannot compare String and Int"
    True @=? r

 , "int != str" ~: do
    r <- evalFail "4 != \"abc\"" "Cannot compare Int and String"
    True @=? r

 , "bool != int" ~: do
    r <- evalTest "fals != 4"
    "adevarat" @=? r

 , "int != bool" ~: do
    r <- evalTest "4 != fals"
    "adevarat" @=? r

 , "str != float" ~: do
    r <- evalFail "\"abc\" != 4.2" "Cannot compare String and Float"
    True @=? r

 , "float != str" ~: do
    r <- evalFail "4.2 != \"abc\"" "Cannot compare Float and String"
    True @=? r

 , "bool != float" ~: do
    r <- evalTest "fals != 4.2"
    "adevarat" @=? r

 , "float != bool" ~: do
    r <- evalTest "4.2 != fals"
    "adevarat" @=? r

 , "bool != str" ~: do
    r <- evalTest "fals != \"abc\""
    "adevarat" @=? r

 , "str != bool" ~: do
    r <- evalTest "\"abc\" != fals"
    "adevarat" @=? r

 , "1 sau 1" ~: do
    r <- evalFail "1 sau 1" "Or is invalid on Ints"
    True @=? r

 , "1 sau 1.2" ~: do
    r <- evalFail "1 sau 1.2" "Or is invalid on Int and Float"
    True @=? r

 , "1.2 sau 1" ~: do
    r <- evalFail "1.2 sau 1" "Or is invalid on Float and Int"
    True @=? r

 , "\"abc\" sau 1" ~: do
    r <- evalFail "\"abc\" sau 1" "Or is invalid on String and Int"
    True @=? r

 , "1 sau \"abc\"" ~: do
    r <- evalFail "1 sau \"abc\"" "Or is invalid on Int and String"
    True @=? r

 , "fals sau 1" ~: do
    r <- evalFail "fals sau 1" "Or is invalid on Bool and Int"
    True @=? r

 , "1 sau fals" ~: do
    r <- evalFail "1 sau fals" "Or is invalid on Int and Bool"
    True @=? r

 , "1.1 sau 1.1" ~: do
    r <- evalFail "1.1 sau 1.1" "Or is invalid on Floats"
    True @=? r

 , "\"abc\" sau 1.1" ~: do
    r <- evalFail "\"abc\" sau 1.1" "Or is invalid on String and Float"
    True @=? r

 , "1.1 sau \"abc\"" ~: do
    r <- evalFail "1.1 sau \"abc\"" "Or is invalid on Float and String"
    True @=? r

 , "fals sau 11.1" ~: do
    r <- evalFail "fals sau 11.1" "Or is invalid on Bool and Float"
    True @=? r

 , "1.1 sau fals" ~: do
    r <- evalFail "1.1 sau fals" "Or is invalid on Float and Bool"
    True @=? r

 , "\"abc\" sau \"abc\"" ~: do
    r <- evalFail "\"abc\" sau \"abc\"" "Or is invalid on Strings"
    True @=? r

 , "fals sau \"abc\"" ~: do
    r <- evalFail "fals sau \"abc\"" "Or is invalid on Bool and String"
    True @=? r

 , "\"abc\" sau fals" ~: do
    r <- evalFail "\"abc\" sau fals" "Or is invalid on String and Bool"
    True @=? r

 , "bool or bool" ~: do
   r <- evalTest "fals sau adevarat"
   "adevarat" @=? r

 , "str >= int" ~: do
    r <- evalFail "\"abc\" >= 4" "Cannot compare String and Int"
    True @=? r

 , "int >= str" ~: do
    r <- evalFail "4 >= \"abc\"" "Cannot compare Int and String"
    True @=? r

 , "bool >= int" ~: do
    r <- evalFail "fals >= 4" "Cannot compare Bool and Int"
    True @=? r

 , "int >= bool" ~: do
    r <- evalFail "4 >= fals" "Cannot compare Int and Bool"
    True @=? r

 , "str >= float" ~: do
    r <- evalFail "\"abc\" >= 4.2" "Cannot compare String and Float"
    True @=? r

 , "float >= str" ~: do
    r <- evalFail "4.2 >= \"abc\"" "Cannot compare Float and String"
    True @=? r

 , "bool >= float" ~: do
    r <- evalFail "fals >= 4.2" "Cannot compare Bool and Float"
    True @=? r

 , "float >= bool" ~: do
    r <- evalFail "4.2 >= fals" "Cannot compare Float and Bool"
    True @=? r

 , "bool >= str" ~: do
    r <- evalFail "fals >= \"abc\"" "Cannot compare Bool and String"
    True @=? r

 , "str >= bool" ~: do
    r <- evalFail "\"abc\" >= fals" "Cannot compare String and Bool"
    True @=? r

 , "bool >= bool" ~: do
    r <- evalFail "adevarat >= fals" "Cannot compare Bools"
    True @=? r

 , "int >= int" ~: do
    r <- evalTest "1 >= 1"
    "adevarat" @=? r

 , "int >= float" ~: do
    r <- evalTest "1 >= 1.0"
    "adevarat" @=? r

 , "float >= int" ~: do
    r <- evalTest "1.0 >= 1"
    "adevarat" @=? r

 , "float >= float" ~: do
    r <- evalTest "1.5 >= 1.5"
    "adevarat" @=? r

 , "str >= str" ~: do
    r <- evalTest "\"ana are mere\" >= \"ana are mere\""
    "adevarat" @=? r

 , "str > int" ~: do
    r <- evalFail "\"abc\" > 4" "Cannot compare String and Int"
    True @=? r

 , "int > str" ~: do
    r <- evalFail "4 > \"abc\"" "Cannot compare Int and String"
    True @=? r

 , "bool > int" ~: do
    r <- evalFail "fals > 4" "Cannot compare Bool and Int"
    True @=? r

 , "int > bool" ~: do
    r <- evalFail "4 > fals" "Cannot compare Int and Bool"
    True @=? r

 , "str > float" ~: do
    r <- evalFail "\"abc\" > 4.2" "Cannot compare String and Float"
    True @=? r

 , "float > str" ~: do
    r <- evalFail "4.2 > \"abc\"" "Cannot compare Float and String"
    True @=? r

 , "bool > float" ~: do
    r <- evalFail "fals > 4.2" "Cannot compare Bool and Float"
    True @=? r

 , "float > bool" ~: do
    r <- evalFail "4.2 > fals" "Cannot compare Float and Bool"
    True @=? r

 , "bool > str" ~: do
    r <- evalFail "fals > \"abc\"" "Cannot compare Bool and String"
    True @=? r

 , "str > bool" ~: do
    r <- evalFail "\"abc\" > fals" "Cannot compare String and Bool"
    True @=? r

 , "bool > bool" ~: do
    r <- evalFail "adevarat > fals" "Cannot compare Bools"
    True @=? r

 , "int > int" ~: do
    r <- evalTest "1 > 0"
    "adevarat" @=? r

 , "int > float" ~: do
    r <- evalTest "1 > 0.5"
    "adevarat" @=? r

 , "float > int" ~: do
    r <- evalTest "1.5 > 1"
    "adevarat" @=? r

 , "float > float" ~: do
    r <- evalTest "1.5 > 1.3"
    "adevarat" @=? r

 , "str > str" ~: do
    r <- evalTest "\"ana are mere\" > \"ana are pere\""
    "fals" @=? r

 , "str <= int" ~: do
    r <- evalFail "\"abc\" <= 4" "Cannot compare String and Int"
    True @=? r

 , "int <= str" ~: do
    r <- evalFail "4 <= \"abc\"" "Cannot compare Int and String"
    True @=? r

 , "bool <= int" ~: do
    r <- evalFail "fals <= 4" "Cannot compare Bool and Int"
    True @=? r

 , "int <= bool" ~: do
    r <- evalFail "4 <= fals" "Cannot compare Int and Bool"
    True @=? r

 , "str <= float" ~: do
    r <- evalFail "\"abc\" <= 4.2" "Cannot compare String and Float"
    True @=? r

 , "float <= str" ~: do
    r <- evalFail "4.2 <= \"abc\"" "Cannot compare Float and String"
    True @=? r

 , "bool <= float" ~: do
    r <- evalFail "fals <= 4.2" "Cannot compare Bool and Float"
    True @=? r

 , "float <= bool" ~: do
    r <- evalFail "4.2 <= fals" "Cannot compare Float and Bool"
    True @=? r

 , "bool <= str" ~: do
    r <- evalFail "fals <= \"abc\"" "Cannot compare Bool and String"
    True @=? r

 , "str <= bool" ~: do
    r <- evalFail "\"abc\" <= fals" "Cannot compare String and Bool"
    True @=? r

 , "bool <= bool" ~: do
    r <- evalFail "adevarat <= fals" "Cannot compare Bools"
    True @=? r

 , "int <= int" ~: do
    r <- evalTest "1 <= 1"
    "adevarat" @=? r

 , "int <= float" ~: do
    r <- evalTest "1 <= 1.0"
    "adevarat" @=? r

 , "float <= int" ~: do
    r <- evalTest "1.0 <= 1"
    "adevarat" @=? r

 , "float <= float" ~: do
    r <- evalTest "1.3 <= 1.3"
    "adevarat" @=? r

 , "str <= str" ~: do
    r <- evalTest "\"ana are mere\" <= \"ana are mere\""
    "adevarat" @=? r

 , "str < int" ~: do
    r <- evalFail "\"abc\" < 4" "Cannot compare String and Int"
    True @=? r

 , "int < str" ~: do
    r <- evalFail "4 < \"abc\"" "Cannot compare Int and String"
    True @=? r

 , "bool < int" ~: do
    r <- evalFail "fals < 4" "Cannot compare Bool and Int"
    True @=? r

 , "int < bool" ~: do
    r <- evalFail "4 < fals" "Cannot compare Int and Bool"
    True @=? r

 , "str < float" ~: do
    r <- evalFail "\"abc\" < 4.2" "Cannot compare String and Float"
    True @=? r

 , "float < str" ~: do
    r <- evalFail "4.2 < \"abc\"" "Cannot compare Float and String"
    True @=? r

 , "bool < float" ~: do
    r <- evalFail "fals < 4.2" "Cannot compare Bool and Float"
    True @=? r

 , "float < bool" ~: do
    r <- evalFail "4.2 < fals" "Cannot compare Float and Bool"
    True @=? r

 , "bool < str" ~: do
    r <- evalFail "fals < \"abc\"" "Cannot compare Bool and String"
    True @=? r

 , "str < bool" ~: do
    r <- evalFail "\"abc\" < fals" "Cannot compare String and Bool"
    True @=? r

 , "bool < bool" ~: do
    r <- evalFail "adevarat < fals" "Cannot compare Bools"
    True @=? r

 , "int < int" ~: do
    r <- evalTest "1 < 23"
    "adevarat" @=? r

 , "int < float" ~: do
    r <- evalTest "1 < 1.4"
    "adevarat" @=? r

 , "float < int" ~: do
    r <- evalTest "1.0 < 3"
    "adevarat" @=? r

 , "float < float" ~: do
    r <- evalTest "1.3 < 1.4"
    "adevarat" @=? r

 , "str < str" ~: do
    r <- evalTest "\"ana are mere\" < \"ana are pere\""
    "adevarat" @=? r

 , "str % int" ~: do
    r <- evalFail "\"abc\" % 4" "Modulo cannot be applied to String and Int"
    True @=? r

 , "int % str" ~: do
    r <- evalFail "4 % \"abc\"" "Modulo cannot be applied to Int and String"
    True @=? r

 , "bool % int" ~: do
    r <- evalFail "fals % 4" "Modulo cannot be applied to Bool and Int"
    True @=? r

 , "int % bool" ~: do
    r <- evalFail "4 % fals" "Modulo cannot be applied to Int and Bool"
    True @=? r

 , "str % float" ~: do
    r <- evalFail "\"abc\" % 4.2" "Modulo cannot be applied to String and Float"
    True @=? r

 , "float % str" ~: do
    r <- evalFail "4.2 % \"abc\"" "Modulo cannot be applied to Float and String"
    True @=? r

 , "bool % float" ~: do
    r <- evalFail "fals % 4.2" "Modulo cannot be applied to Bool and Float"
    True @=? r

 , "float % bool" ~: do
    r <- evalFail "4.2 % fals" "Modulo cannot be applied to Float and Bool"
    True @=? r

 , "bool % str" ~: do
    r <- evalFail "fals % \"abc\"" "Modulo cannot be applied to Bool and String"
    True @=? r

 , "str % bool" ~: do
    r <- evalFail "\"abc\" % fals" "Modulo cannot be applied to String and Bool"
    True @=? r

 , "bool % bool" ~: do
    r <- evalFail "adevarat % fals" "Modulo cannot be applied to Bools"
    True @=? r

 , "float % float" ~: do
    r <- evalFail "2.3 % 2.4" "Modulo cannot be applied to Floats"
    True @=? r

 , "str % str" ~: do
    r <- evalFail "\"abc\" % \"cba\"" "Modulo cannot be applied to Strings"
    True @=? r

 , "int % float" ~: do
    r <- evalFail "2 % 2.4" "Modulo cannot be applied to Int and Float"
    True @=? r

 , "float % int" ~: do
    r <- evalFail "2.3 % 2" "Modulo cannot be applied to Float and Int"
    True @=? r

 , "str / int" ~: do
    r <- evalFail "\"abc\" / 4" "Cannot divide String by Int"
    True @=? r

 , "int / str" ~: do
    r <- evalFail "4 / \"abc\"" "Cannot divide Int by String"
    True @=? r

 , "bool / int" ~: do
    r <- evalFail "fals / 4" "Cannot divide Bool by Int"
    True @=? r

 , "int / bool" ~: do
    r <- evalFail "4 / fals" "Cannot divide Int by Bool"
    True @=? r

 , "str / float" ~: do
    r <- evalFail "\"abc\" / 4.2" "Cannot divide String by Float"
    True @=? r

 , "float / str" ~: do
    r <- evalFail "4.2 / \"abc\"" "Cannot divide Float by String"
    True @=? r

 , "bool / float" ~: do
    r <- evalFail "fals / 4.2" "Cannot divide Bool by Float"
    True @=? r

 , "float / bool" ~: do
    r <- evalFail "4.2 / fals" "Cannot divide Float by Bool"
    True @=? r

 , "bool / str" ~: do
    r <- evalFail "fals / \"abc\"" "Cannot divide Bool by String"
    True @=? r

 , "str / bool" ~: do
    r <- evalFail "\"abc\" / fals" "Cannot divide String by Bool"
    True @=? r

 , "bool / bool" ~: do
    r <- evalFail "adevarat / fals" "Cannot divide Bools"
    True @=? r

 , "float / float" ~: do
    r <- evalTest "2.3 / 2.4 > 0.95 si 2.3 / 2.4 < 0.96"
    "adevarat" @=? r

 , "str / str" ~: do
    r <- evalFail "\"abc\" / \"cba\"" "Cannot divide Strings"
    True @=? r

 , "int / float" ~: do
    r <- evalTest "2 / 2.4 > 0.83 si 2/2.4 < 0.84"
    "adevarat" @=? r

 , "float / int" ~: do
    r <- evalTest "2.3 / 2"
    "1.15" @=? r

 , "str * int" ~: do
    r <- evalFail "\"abc\" * 4" "Cannot multiply String and Int"
    True @=? r

 , "int * str" ~: do
    r <- evalFail "4 * \"abc\"" "Cannot multiply Int and String"
    True @=? r

 , "bool * int" ~: do
    r <- evalFail "fals * 4" "Cannot multiply Bool and Int"
    True @=? r

 , "int * bool" ~: do
    r <- evalFail "4 * fals" "Cannot multiply Int and Bool"
    True @=? r

 , "str * float" ~: do
    r <- evalFail "\"abc\" * 4.2" "Cannot multiply String and Float"
    True @=? r

 , "float * str" ~: do
    r <- evalFail "4.2 * \"abc\"" "Cannot multiply Float and String"
    True @=? r

 , "bool * float" ~: do
    r <- evalFail "fals * 4.2" "Cannot multiply Bool and Float"
    True @=? r

 , "float * bool" ~: do
    r <- evalFail "4.2 * fals" "Cannot multiply Float and Bool"
    True @=? r

 , "bool * str" ~: do
    r <- evalFail "fals * \"abc\"" "Cannot multiply Bool and String"
    True @=? r

 , "str * bool" ~: do
    r <- evalFail "\"abc\" * fals" "Cannot multiply String and Bool"
    True @=? r

 , "bool * bool" ~: do
    r <- evalFail "adevarat * fals" "Cannot multiply Bools"
    True @=? r

 , "float * float" ~: do
    r <- evalTest "2.3 * 2.4"
    "5.52" @=? r

 , "str * str" ~: do
    r <- evalFail "\"abc\" * \"cba\"" "Cannot multiply Strings"
    True @=? r

 , "int * float" ~: do
    r <- evalTest "2 * 2.4"
    "4.8" @=? r

 , "float * int" ~: do
    r <- evalTest "2.3 * 2"
    "4.6" @=? r

 , "str - int" ~: do
    r <- evalFail "\"abc\" - 4" "Cannot subtract Int from String"
    True @=? r

 , "int - str" ~: do
    r <- evalFail "4 - \"abc\"" "Cannot subtract String from Int"
    True @=? r

 , "bool - int" ~: do
    r <- evalFail "fals - 4" "Cannot subtract Int from Bool"
    True @=? r

 , "int - bool" ~: do
    r <- evalFail "4 - fals" "Cannot subtract Bool from Int"
    True @=? r

 , "str - float" ~: do
    r <- evalFail "\"abc\" - 4.2" "Cannot subtract Float from String"
    True @=? r

 , "float - str" ~: do
    r <- evalFail "4.2 - \"abc\"" "Cannot subtract String from Float"
    True @=? r

 , "bool - float" ~: do
    r <- evalFail "fals - 4.2" "Cannot subtract Float from Bool"
    True @=? r

 , "float - bool" ~: do
    r <- evalFail "4.2 - fals" "Cannot subtract Bool from Float"
    True @=? r

 , "bool - str" ~: do
    r <- evalFail "fals - \"abc\"" "Cannot subtract String from Bool"
    True @=? r

 , "str - bool" ~: do
    r <- evalFail "\"abc\" - fals" "Cannot subtract Bool from String"
    True @=? r

 , "bool - bool" ~: do
    r <- evalFail "adevarat - fals" "Cannot subtract Bools"
    True @=? r

 , "float - float" ~: do
    r <- evalTest "2.3 - 2.4 < 0 si 2.3 - 2.4 > -0.5"
    "adevarat" @=? r

 , "str - str" ~: do
    r <- evalTest "\"bcabca\" - \"abc\""
    "\"bca\"" @=? r

 , "int - float" ~: do
    r <- evalTest "2 - 2.4 < 0 si 2 - 2.4 > -0.5"
    "adevarat" @=? r

 , "float - int" ~: do
    r <- evalTest "2.3 - 2 > 0.2 si 2.3 - 2 < 0.4"
    "adevarat" @=? r

 , "str + int" ~: do
    r <- evalTest "\"abc\" + 4"
    "\"abc4\"" @=? r

 , "int + str" ~: do
    r <- evalTest "4 + \"abc\""
    "\"4abc\"" @=? r

 , "bool + int" ~: do
    r <- evalFail "fals + 4" "Cannot add together Bool and Int"
    True @=? r

 , "int + bool" ~: do
    r <- evalFail "4 + fals" "Cannot add together Int and Bool"
    True @=? r

 , "str + float" ~: do
    r <- evalTest "\"abc\" + 4.2"
    "\"abc4.2\"" @=? r

 , "float + str" ~: do
    r <- evalTest "4.2 + \"abc\""
    "\"4.2abc\"" @=? r

 , "bool + float" ~: do
    r <- evalFail "fals + 4.2" "Cannot add together Bool and Float"
    True @=? r

 , "float + bool" ~: do
    r <- evalFail "4.2 + fals" "Cannot add together Float and Bool"
    True @=? r

 , "bool + str" ~: do
    r <- evalTest "adevarat + \"abc\""
    "\"adevaratabc\"" @=? r

 , "bool_false + str" ~: do
    r <- evalTest "fals + \"abc\""
    "\"falsabc\"" @=? r

 , "str + bool" ~: do
    r <- evalTest "\"abc\" + fals"
    "\"abcfals\"" @=? r

 , "str + bool_true" ~: do
    r <- evalTest "\"abc\" + adevarat"
    "\"abcadevarat\"" @=? r

 , "bool + bool" ~: do
    r <- evalFail "adevarat + fals" "Cannot add together Bools"
    True @=? r

 , "float + float" ~: do
    r <- evalTest "2.3 + 2.4 > 4.6 si 2.3+2.4<4.8"
    "adevarat" @=? r

 , "str + str" ~: do
    r <- evalTest "\"abc\" + \"cba\""
    "\"abccba\"" @=? r

 , "int + float" ~: do
    r <- evalTest "2 + 2.4"
    "4.4" @=? r

 , "float + int" ~: do
    r <- evalTest "2.3 + 2"
    "4.3" @=? r

 , "string index" ~: do
    r <- evalTest "a=\"abc\" a[2]"
    "\"c\"" @=? r

 , "string index inside list" ~: do
    r <- evalTest "a={\"abc\"} a[0][2]"
    "\"c\"" @=? r

 , "string index modification" ~: do
    r <- evalTest "a=\"abc\" a[2]=\"d\" a"
    "\"abd\"" @=? r

 , "string index modification in nested list" ~: do
    r <- evalTest "a={1,{3,\"abc\"},2} a[1][1][2]=\"d\" a"
    "{1, {3, \"abd\"}, 2}" @=? r

 , "string modification in nested list" ~: do
    r <- evalTest "a={1,{3,\"abc\"},2} a[1][1]=\"d\" a"
    "{1, {3, \"d\"}, 2}" @=? r

 , "string index in nested list" ~: do
    r <- evalTest "a={1,{3,\"abc\"},2} a[1][1][2]"
    "\"c\"" @=? r

 , "string index inside list modification" ~: do
    r <- evalTest "a={\"abc\"} a[0][2]=\"d\" a"
    "{\"abd\"}" @=? r

 , "invalid string index" ~: do
    r <- evalFail "a=\"abc\" a[-1]" "Invalid string index: a[-1]"
    True @=? r

 , "invalid nested string index" ~: do
    r <- evalFail "a={\"abc\"} a[0][-1]" "Invalid nested string index: -1"
    True @=? r

 , "invalid variable index" ~: do
    r <- evalFail "a=1 a[0]" "Only Lists and Strings can be indexed"
    True @=? r

 , "index in char" ~: do
    r <- evalFail "a=\"abc\" a[0][0]" "Multiple indexing can be applied only to Lists"
    True @=? r

 , "non-integer string index" ~: do
    r <- evalFail "a=\"abc\" a[4.2]" "Strings can be indexed only with Integer evaluating expressions"
    True @=? r

 , "non-integer nested string index" ~: do
    r <- evalFail "a={\"abc\"} a[0][4.2]" "Strings can be indexed only with Integer evaluating expressions"
    True @=? r

 , "inf while" ~: do
    r <- evalTest "a=0 cattimp adevarat executa daca a>5 atunci stop altfel a=a+1 sfdaca sfcattimp"
    "" @=? r

 , "inf for" ~: do
    r <- evalTest "pt a=0;; executa stop a=100 sfpt a"
    "0" @=? r

 , "inf nested for" ~: do
    r <- evalTest "pt a=0;a<5;a=a+1 executa pt ;; executa stop a=100 sfpt sfpt a"
    "5" @=? r

 , "inf while, finite while" ~: do
    r <- evalTest "a=0 cattimp adevarat executa daca a>5 atunci stop altfel a=a+1 sfdaca sfcattimp a=1 cattimp a<5 executa a=a+1 sfcattimp a"
    "5" @=? r

 , "inf for, finite for" ~: do
    r <- evalTest "pt ;; executa stop sfpt pt a=0; a<5; a=a+1 executa sfpt a"
    "5" @=? r

 , "inf while, no variable modification after stop" ~: do
    r <- evalTest "a=0 cattimp adevarat executa stop a=100 sfcattimp a"
    "0" @=? r

 , "stop has no effect in function body" ~: do
    r <- evalTest "func a() stop 1 sffunc a()"
    "1" @=? r

 , "basic function call" ~: do
    r <- evalTest "func a() 1 sffunc a()"
    "1" @=? r

 , "basic function call with an arg" ~: do
    r <- evalTest "func a(b) b sffunc b=3 a(2)"
    "2" @=? r

 , "stop has no effect in if" ~: do
    r <- evalTest "a=1 daca 1==1 atunci stop a=42 sfdaca a"
    "42" @=? r

 , "basic ret" ~: do
    r <- evalTest "func b() ret 2 ret 3 sffunc b()"
    "2" @=? r

 , "no access to undefined var" ~: do
    r <- evalTest "a=1 func b() a=4 ret a sffunc b()+a"
    "5" @=? r

 , "func call as ret value" ~: do
    r <- evalTest "func a() ret b()+1 sffunc func b() ret 4 sffunc a()"
    "5" @=? r

 , "mutual recursion" ~: do
    r <- evalTest "func is_even(n) daca n == 0 atunci ret adevarat altfel ret is_odd(n-1) sfdaca sffunc func is_odd(n) daca n == 0 atunci ret fals altfel ret is_even(n-1) sfdaca sffunc is_even(4)"
    "adevarat" @=? r

 , "simple func call with changing arg" ~: do
    r <- evalTest "func foo(n) ret a(n-1) sffunc func a(n) ret n*2 sffunc foo(3)"
    "4" @=? r

 , "ret goes out of the function" ~: do
    r <- evalTest "func foo(n) daca n == 4 atunci ret adevarat sfdaca ret n-1 sffunc foo(4)"
    "adevarat" @=? r

 , "ret after if in func" ~: do
    r <- evalTest "func foo(n) daca n == 0 atunci ret adevarat altfel ret n-1 sfdaca sffunc foo(4)"
    "3" @=? r

 , "call a number" ~: do
    r <- evalFail "a=1 a()" "Only functions are callable"
    True @=? r

 , "different number of args" ~: do
    r <- evalFail "func a() ret 2 sffunc a(1)" "Trying to pass 1 args to a function that takes 0"
    True @=? r

 , "func name that has a param with the same name" ~: do
    r <- evalTest "func a(a) ret a sffunc a(42)"
    "42" @=? r

 , "function in list" ~: do
    r <- evalTest "a={1, func (a) ret a[1]-1 sffunc, 3} a[1]({42,43})"
    "42" @=? r

 , "lambda assigned to variable" ~: do
    r <- evalTest "a=func(b) ret 3*b sffunc a(14)"
    "42" @=? r

 , "simple callback" ~: do
    r <- evalTest "func a(callback) ret callback(39) sffunc a(func(x) ret x+3 sffunc)"
    "42" @=? r

 , "func as retval" ~: do
    r <- evalTest "func a(callback) ret callback(39) sffunc a(func(x) ret x+3 sffunc)"
    "42" @=? r

 , "simple closure called directly" ~: do
    r <- evalTest "func a(x) ret func (b) ret x+b sffunc sffunc a(3)(2)"
    "5" @=? r

 , "simple closure called through variable" ~: do
    r <- evalTest "func a(x) ret func (b) ret x+b sffunc sffunc b=a(3) b(2)"
    "5"  @=? r

 , "func def inside func def" ~: do
    r <- evalTest "func a() func b() ret 42 sffunc ret b() sffunc a()"
    "42" @=? r

 , "doubly defined function" ~: do
    r <- evalFail "func a() ret 1 sffunc func a() ret 42 sffunc" "The function name \"a\" shadows another name in the current scope"
    True @=? r

 , "parse fail" ~: do
    r <- evalFail "(" "unexpected end of input"
    True @=? r

 , "full program, no args" ~: do
     r <- evalProgram "func main() scrie(42) sffunc" []
     "OK" @=? r

 , "full program, with args" ~: do
    r <- evalProgram "func main(argv) scrie(\"args:\", argv) sffunc" ["1", "2", "3"]
    "OK" @=? r

 , "full program, with args and two functions" ~: do
    r <- evalProgram "func a() ret 42 sffunc func main(argv) ret 1 sffunc" ["1", "2", "3"]
    "OK" @=? r

 , "scrie error" ~: do
     r <- evalProgram "func main() scrie()() sffunc" []
     "Error: scrie takes a variable number of arguments" @=? r

 , "citeste error" ~: do
     r <- evalProgram "func main() citeste(123) sffunc" []
     "Error: citeste takes zero arguments" @=? r

 , "string to int cast" ~: do
    r <- evalTest "int(\"42\")"
    "42" @=? r

 , "int error" ~: do
    r <- evalFail "int(42, 43)" "int takes a single String argument"
    True @=? r

 , "list length" ~: do
    r <- evalTest "lung({\"42\", 2})"
    "2" @=? r

 , "list length error" ~: do
    r <- evalFail "lung(2)" "lung takes a single List argument"
    True @=? r

 , "func args" ~: do
    r <- evalTest "func foo(a,b,c) ret a+b+c sffunc foo(1,2,3)"
    "6" @=? r

 , "int partial parse fail" ~: do
    r <- evalTest "int(\"2asd\")"
    "fals" @=? r

 , "int parse fail" ~: do
    r <- evalTest "int(\"asd\")"
    "fals" @=? r

 , "named closure, called directly" ~: do
    r <- evalTest "func foo(x) ret func xyz(b) ret x+b sffunc sffunc foo(123)(123)"
    "246" @=? r

 , "display bultin func" ~: do
    r <- evalTest "int"
    "<builtin func>" @=? r

 , "display bultin io func" ~: do
    r <- evalTest "scrie"
    "<builtin func>" @=? r

 , "display user defined func" ~: do
    r <- evalTest "func foo(a,b) ret a+b sffunc foo"
    "<user defined func taking 2 args>" @=? r

 , "float partial parse fail" ~: do
    r <- evalTest "float(\"2.2asd\")"
    "fals" @=? r

 , "float parse fail" ~: do
    r <- evalTest "float(\"asd\")"
    "fals" @=? r

 , "string to float cast" ~: do
    r <- evalTest "float(\"4.2\")"
    "4.2" @=? r

 , "float error" ~: do
    r <- evalFail "float(4.2, 43)" "float takes a single String argument"
    True @=? r

 , "apply error" ~: do
    r <- evalFail "apply(1, 2, 3)" "apply takes two arguments"
    True @=? r

 , "apply" ~: do
    r <- evalTest "a=apply(func foo(a,b) ret a+b sffunc, 2) a(3)"
    "5" @=? r

 , "open, write, close" ~: do
    r <- evalTest "f=deschide(\"testfile.txt\", \"w\") fscrie(f, 1, 2, \"12\") inchide(f)"
    "" @=? r

 , "open error" ~: do
    r <- evalFail "deschide()" "deschide takes two arguments, a String as the file path and a mode describing how to open the file"
    True @=? r

 , "close error" ~: do
    r <- evalFail "inchide()" "inchide takes a single argument, the result of deschide"
    True @=? r

 , "fwrite error" ~: do
    r <- evalFail "fscrie()" "fscrie takes a variable number of arguments, the first one should be the file to write to"
    True @=? r

 , "open error" ~: do
    r <- evalFail "deschide(\"1234567890\", \"r\")" "File 1234567890 doesn't exist"
    True @=? r

 , "open (read), close" ~: do
    r <- evalTest "f=deschide(\"testfile.txt\", \"r\") inchide(f)"
    "" @=? r

 , "open (append), close" ~: do
    r <- evalTest "f=deschide(\"testfile.txt\", \"a\") inchide(f)"
    "" @=? r

 , "open, read, close" ~: do
    r <- evalTest "f=deschide(\"testfile.txt\", \"r\") a=fciteste(f) inchide(f) a"
    "\"1212\"" @=? r

 , "read fail" ~: do
    r <- evalFail "fciteste(2)" "fciteste takes a single argument, the result of deschide"
    True @=? r

 , "open, read until EOF, close" ~: do
    r <- evalTest "f=deschide(\"testfile.txt\", \"r\") fciteste(f) a=fciteste(f) inchide(f) a"
    "fals" @=? r

 , "strlen" ~: do
    r <- evalTest "lung(\"abcd\")"
    "4" @=? r

 , "sleep" ~: do
    r <- evalTest "sleep(2)"
    "" @=? r

 , "sleep error" ~: do
    r <- evalFail "sleep(2,2,3)" "sleep takes a single int argument"
    True @=? r

 , "ceiling error" ~: do
    r <- evalFail "ceiling(2,2,3)" "ceiling takes a single int/float argument"
    True @=? r

 , "floor error" ~: do
    r <- evalFail "floor(2,2,3)" "floor takes a single int/float argument"
    True @=? r

  , "floor int" ~: do
     r <- evalTest "floor(42)"
     "42" @=? r

  , "floor float" ~: do
     r <- evalTest "floor(42.6)"
     "42" @=? r

 , "ceiling int" ~: do
     r <- evalTest "ceiling(42)"
     "42" @=? r

 , "ceiling float" ~: do
    r <- evalTest "ceiling(41.2)"
    "42" @=? r

 , "list == bool" ~: do
    r <- evalTest "{1,2,3} == fals"
    "fals" @=? r

 , "bool == list" ~: do
    r <- evalTest "fals == {1,2,3}"
    "fals" @=? r

 , "list != bool" ~: do
    r <- evalTest "{1,2,3} != fals"
    "adevarat" @=? r

 , "bool != list" ~: do
    r <- evalTest "fals != {1,2,3}"
    "adevarat" @=? r

 , "shortcircuit and" ~: do
    r <- evalTest "fals si scrie(2)"
    "fals" @=? r

 , "shortcircuit and - error" ~: do
    r <- evalFail "int(\"2\") si scrie(2)" "And is valid only on Bools"
    True @=? r

 , "shortcircuit and - error on the right side" ~: do
    r <- evalFail "adevarat si int(\"2\")" "And is valid only on Bools"
    True @=? r

 , "shortcircuit or" ~: do
    r <- evalTest "adevarat sau scrie(2)"
    "adevarat" @=? r

 , "shortcircuit or - error" ~: do
    r <- evalFail "int(\"2\") sau scrie(2)" "Or is valid only on Bools"
    True @=? r

 , "shortcircuit or - error on the right side" ~: do
    r <- evalFail "fals sau int(\"2\")" "Or is valid only on Bools"
    True @=? r

 , "struct shadowing func" ~: do
    r <- evalProgram "func point() ret 42 sffunc struct point x=0 y=0 sfstruct" []
    "Error: Cannot declare type point since it will shadow other names" @=? r

 , "struct ctor simple call" ~: do
    r <- evalProgram "struct point x=0 y=0 sfstruct func main() point() sffunc" []
    "Error: Cannot declare type point since it will shadow other names" @=? r

 , "struct ctor + direct member access" ~: do
    r <- evalProgram "struct point x=0 y=0 sfstruct func main() scrie(point().x) sffunc" []
    "Error: Cannot declare type point since it will shadow other names" @=? r

 , "non-struct member access" ~: do
    r <- evalProgram "func main() a=2 scrie(a.x) sffunc" []
    "Error: Cannot declare type point since it will shadow other names" @=? r
 ]
