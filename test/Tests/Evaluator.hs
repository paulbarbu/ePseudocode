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
    "{1, {{5, 6}, 3}, 4}" ~=? evalTest "b=0 a={1, {2, 3}, 4} a[b+1][b] = {5,6} a"

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
    "adevarat" ~=? evalTest "a=0 b={10, 1} cattimp b[a] > 0 executa b[0]=b[0]-1 b[1]=b[1]*2 sfcattimp 2**10 == b[1] si 0 == b[0]"

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
    "{1, {2, {{5, 6}, 5}}, 4}" ~=? evalTest "b=1 a={1, {2, {3, 5}}, 4} a[1][b][0] = {5,6} a"

 , "nested list 2nd level index access with variable" ~:
    "3" ~=? evalTest "b=1 a={1, {2, {3, 5}}, 4} a[1][b][0]"

 , "nested list 3rd level index access with variables" ~:
    "3" ~=? evalTest "b=0 a={1, {{3, 5}}, 4} a[1][b][b]"

 , "nested list 2nd level index access" ~:
    "3" ~=? evalTest "a={1, {2, 3}, 4} a[1][1]"

 , "string" ~:
    "\"abc\"" ~=? evalTest "a=\"foobar\" \"abc\""

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
    "\"cba\"" ~=? evalTest "-\"abc\""

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

 , "for 1;1;1" ~:
    "5" ~=? evalTest "b=-1 pt a=b+1;a<5;a=a+1 executa sfpt a"

 , "for 0;1;1" ~:
    "5" ~=? evalTest "a=0 pt ;a<5;a=a+1 executa sfpt a"

 , "for 0;1;0" ~:
    "5" ~=? evalTest "a=0 pt ;a<5; executa a=a+1 sfpt a"

 , "for 1;1;0" ~:
    "5" ~=? evalTest "pt a=0;a<5; executa a=a+1 sfpt a"

 , "for error condition" ~: evalFail "pt a=0;a<adevarat; executa a=a+1 sfpt a" "Cannot compare Int and Bool"

 , "lists lt" ~:
    "adevarat" ~=? evalTest "a=1 b=2 {{a,2},4} < {{b,3},5}"

 , "lists le" ~:
    "adevarat" ~=? evalTest "a=1 b=2 {a,b, 42} <= {b,4}"

 , "lists gt" ~:
    "fals" ~=? evalTest "a=1 b=2 {a,3} > {b,4}"

 , "lists ge" ~:
    "adevarat" ~=? evalTest "a=7 b=2 {a,2} >= {b,4}"

 , "lists eq" ~:
    "fals" ~=? evalTest "a=7 b=2 {a,2} == {b,4}"

 , "lists neq" ~:
    "adevarat" ~=? evalTest "a=7 b=3 {a,2} != {b,4}"

 , "remove all elements from list, with variables" ~:
    "{3, 3}" ~=? evalTest "a=7 b=3 {a, a, 3, b, 7} - a"

 , "remove all elements from list, no variables" ~:
    "{7, 7}" ~=? evalTest "{3,3,3,7,3,7}-3"

 , "simple if with env" ~:
    "41.2" ~=? evalTest "a=42.2 daca a==a atunci a=a-1 sfdaca a"

 , "false simple if with env" ~:
    "42" ~=? evalTest "a=42 daca a!=a atunci a=a-1 sfdaca a"

 , "complete if with env" ~:
    "43" ~=? evalTest "a=42 daca a!=a atunci a=a-1 altfel a=a+1 sfdaca a"

 , "float with env" ~:
    "43.2" ~=? evalTest "a=43.2 b=-(-a)"

 , "list concatenation" ~:
    "{1, 2, 3, 4}" ~=? evalTest "a=4 b=2 {1,b} + {3,a}"

 , "item prepend to list" ~:
    "{1.1, 2, 3, 4}" ~=? evalTest "a=1.1 a + {2, 3,4}"

 , "item append to list" ~:
    "{1, 2, 3, \"4\"}" ~=? evalTest "b=\"4\" {1,2,3} + b"

 , "int ** float" ~:
    "adevarat" ~=? evalTest "3**4.2 < 101 si 3**4.2 > 100"

 , "float ** int" ~:
    "adevarat" ~=? evalTest "4.2**3 < 75 si 4.2**3 > 74"

 , "str ** int" ~: evalFail "\"abc\" ** 3" "Cannot raise String and Int"

 , "int ** str" ~: evalFail "3 ** \"abc\"" "Cannot raise Int and String"

 , "bool ** int" ~: evalFail "fals ** 3" "Cannot raise Bool and Int"

 , "int ** bool" ~: evalFail "3 ** fals" "Cannot raise Int and Bool"

 , "float ** float" ~:
    "adevarat" ~=? evalTest "3.3**3.3 > 51 si 3.3**3.3 < 52"

 , "str ** float" ~: evalFail "\"abc\" ** 4.3" "Cannot raise String and Float"

 , "float ** str" ~: evalFail "4.3 ** \"abc\"" "Cannot raise Float and String"

 , "bool ** float" ~: evalFail "fals ** 4.3" "Cannot raise Bool and Float"

 , "float ** bool" ~: evalFail "4.3 ** fals" "Cannot raise Float and Bool"

 , "str ** str" ~: evalFail "\"foo\" ** \"abc\"" "Cannot raise Strings"

 , "bool ** str" ~: evalFail "fals ** \"abc\"" "Cannot raise Bool and String"

 , "str ** bool" ~: evalFail "\"abc\" ** fals" "Cannot raise String and Bool"

 , "bool ** bool" ~: evalFail "fals ** adevarat" "Cannot raise Bools"

 , "int == float" ~:
    "adevarat" ~=? evalTest "1 == 1.0"

 , "str == str" ~:
    "adevarat" ~=? evalTest "\"abc\" == \"abc\""

 , "str == int" ~: evalFail "\"abc\" == 4" "Cannot compare String and Int"

 , "int == str" ~: evalFail "4 == \"abc\"" "Cannot compare Int and String"

 , "bool == int" ~: evalFail "fals == 4" "Cannot compare Bool and Int"

 , "int == bool" ~: evalFail "4 == fals" "Cannot compare Int and Bool"

 , "str == float" ~: evalFail "\"abc\" == 4.2" "Cannot compare String and Float"

 , "float == str" ~: evalFail "4.2 == \"abc\"" "Cannot compare Float and String"

 , "bool == float" ~: evalFail "fals == 4.2" "Cannot compare Bool and Float"

 , "float == bool" ~: evalFail "4.2 == fals" "Cannot compare Float and Bool"

 , "bool == str" ~: evalFail "fals == \"abc\"" "Cannot compare Bool and String"

 , "str == bool" ~: evalFail "\"abc\" == fals" "Cannot compare String and Bool"

 , "int != float" ~:
    "adevarat" ~=? evalTest "1 != 1.2"

 , "float != int" ~:
    "adevarat" ~=? evalTest "1.2 != 1"

 , "float != float" ~:
    "adevarat" ~=? evalTest "1.5 != 1.2"

 , "str != str" ~:
    "fals" ~=? evalTest "\"foo\" != \"foo\""

 , "bool != bool" ~:
    "adevarat" ~=? evalTest "fals != adevarat"

 , "str != int" ~: evalFail "\"abc\" != 4" "Cannot compare String and Int"

 , "int != str" ~: evalFail "4 != \"abc\"" "Cannot compare Int and String"

 , "bool != int" ~: evalFail "fals != 4" "Cannot compare Bool and Int"

 , "int != bool" ~: evalFail "4 != fals" "Cannot compare Int and Bool"

 , "str != float" ~: evalFail "\"abc\" != 4.2" "Cannot compare String and Float"

 , "float != str" ~: evalFail "4.2 != \"abc\"" "Cannot compare Float and String"

 , "bool != float" ~: evalFail "fals != 4.2" "Cannot compare Bool and Float"

 , "float != bool" ~: evalFail "4.2 != fals" "Cannot compare Float and Bool"

 , "bool != str" ~: evalFail "fals != \"abc\"" "Cannot compare Bool and String"

 , "str != bool" ~: evalFail "\"abc\" != fals" "Cannot compare String and Bool"

 , "1 sau 1" ~: evalFail "1 sau 1" "Or is invalid on Ints"

 , "1 sau 1.2" ~: evalFail "1 sau 1.2" "Or is invalid on Int and Float"

 , "1.2 sau 1" ~: evalFail "1.2 sau 1" "Or is invalid on Float and Int"

 , "\"abc\" sau 1" ~: evalFail "\"abc\" sau 1" "Or is invalid on String and Int"

 , "1 sau \"abc\"" ~: evalFail "1 sau \"abc\"" "Or is invalid on Int and String"

 , "fals sau 1" ~: evalFail "fals sau 1" "Or is invalid on Bool and Int"

 , "1 sau fals" ~: evalFail "1 sau fals" "Or is invalid on Int and Bool"

 , "1.1 sau 1.1" ~: evalFail "1.1 sau 1.1" "Or is invalid on Floats"

 , "\"abc\" sau 1.1" ~: evalFail "\"abc\" sau 1.1" "Or is invalid on String and Float"

 , "1.1 sau \"abc\"" ~: evalFail "1.1 sau \"abc\"" "Or is invalid on Float and String"

 , "fals sau 11.1" ~: evalFail "fals sau 11.1" "Or is invalid on Bool and Float"

 , "1.1 sau fals" ~: evalFail "1.1 sau fals" "Or is invalid on Float and Bool"

 , "\"abc\" sau \"abc\"" ~: evalFail "\"abc\" sau \"abc\"" "Or is invalid on Strings"

 , "fals sau \"abc\"" ~: evalFail "fals sau \"abc\"" "Or is invalid on Bool and String"

 , "\"abc\" sau fals" ~: evalFail "\"abc\" sau fals" "Or is invalid on String and Bool"

 , "bool or bool" ~:
  "adevarat" ~=? evalTest "fals sau adevarat"

 , "str >= int" ~: evalFail "\"abc\" >= 4" "Cannot compare String and Int"

 , "int >= str" ~: evalFail "4 >= \"abc\"" "Cannot compare Int and String"

 , "bool >= int" ~: evalFail "fals >= 4" "Cannot compare Bool and Int"

 , "int >= bool" ~: evalFail "4 >= fals" "Cannot compare Int and Bool"

 , "str >= float" ~: evalFail "\"abc\" >= 4.2" "Cannot compare String and Float"

 , "float >= str" ~: evalFail "4.2 >= \"abc\"" "Cannot compare Float and String"

 , "bool >= float" ~: evalFail "fals >= 4.2" "Cannot compare Bool and Float"

 , "float >= bool" ~: evalFail "4.2 >= fals" "Cannot compare Float and Bool"

 , "bool >= str" ~: evalFail "fals >= \"abc\"" "Cannot compare Bool and String"

 , "str >= bool" ~: evalFail "\"abc\" >= fals" "Cannot compare String and Bool"

 , "bool >= bool" ~: evalFail "adevarat >= fals" "Cannot compare Bools"

 , "int >= int" ~:
    "adevarat" ~=? evalTest "1 >= 1"

 , "int >= float" ~:
    "adevarat" ~=? evalTest "1 >= 1.0"

 , "float >= int" ~:
    "adevarat" ~=? evalTest "1.0 >= 1"

 , "float >= float" ~:
    "adevarat" ~=? evalTest "1.5 >= 1.5"

 , "str >= str" ~:
    "adevarat" ~=? evalTest "\"ana are mere\" >= \"ana are mere\""

 , "str > int" ~: evalFail "\"abc\" > 4" "Cannot compare String and Int"

 , "int > str" ~: evalFail "4 > \"abc\"" "Cannot compare Int and String"

 , "bool > int" ~: evalFail "fals > 4" "Cannot compare Bool and Int"

 , "int > bool" ~: evalFail "4 > fals" "Cannot compare Int and Bool"

 , "str > float" ~: evalFail "\"abc\" > 4.2" "Cannot compare String and Float"

 , "float > str" ~: evalFail "4.2 > \"abc\"" "Cannot compare Float and String"

 , "bool > float" ~: evalFail "fals > 4.2" "Cannot compare Bool and Float"

 , "float > bool" ~: evalFail "4.2 > fals" "Cannot compare Float and Bool"

 , "bool > str" ~: evalFail "fals > \"abc\"" "Cannot compare Bool and String"

 , "str > bool" ~: evalFail "\"abc\" > fals" "Cannot compare String and Bool"

 , "bool > bool" ~: evalFail "adevarat > fals" "Cannot compare Bools"

 , "int > int" ~:
    "adevarat" ~=? evalTest "1 > 0"

 , "int > float" ~:
    "adevarat" ~=? evalTest "1 > 0.5"

 , "float > int" ~:
    "adevarat" ~=? evalTest "1.5 > 1"

 , "float > float" ~:
    "adevarat" ~=? evalTest "1.5 > 1.3"

 , "str > str" ~:
    "fals" ~=? evalTest "\"ana are mere\" > \"ana are pere\""

 , "str <= int" ~: evalFail "\"abc\" <= 4" "Cannot compare String and Int"

 , "int <= str" ~: evalFail "4 <= \"abc\"" "Cannot compare Int and String"

 , "bool <= int" ~: evalFail "fals <= 4" "Cannot compare Bool and Int"

 , "int <= bool" ~: evalFail "4 <= fals" "Cannot compare Int and Bool"

 , "str <= float" ~: evalFail "\"abc\" <= 4.2" "Cannot compare String and Float"

 , "float <= str" ~: evalFail "4.2 <= \"abc\"" "Cannot compare Float and String"

 , "bool <= float" ~: evalFail "fals <= 4.2" "Cannot compare Bool and Float"

 , "float <= bool" ~: evalFail "4.2 <= fals" "Cannot compare Float and Bool"

 , "bool <= str" ~: evalFail "fals <= \"abc\"" "Cannot compare Bool and String"

 , "str <= bool" ~: evalFail "\"abc\" <= fals" "Cannot compare String and Bool"

 , "bool <= bool" ~: evalFail "adevarat <= fals" "Cannot compare Bools"

 , "int <= int" ~:
    "adevarat" ~=? evalTest "1 <= 1"

 , "int <= float" ~:
    "adevarat" ~=? evalTest "1 <= 1.0"

 , "float <= int" ~:
    "adevarat" ~=? evalTest "1.0 <= 1"

 , "float <= float" ~:
    "adevarat" ~=? evalTest "1.3 <= 1.3"

 , "str <= str" ~:
    "adevarat" ~=? evalTest "\"ana are mere\" <= \"ana are mere\""

 , "str < int" ~: evalFail "\"abc\" < 4" "Cannot compare String and Int"

 , "int < str" ~: evalFail "4 < \"abc\"" "Cannot compare Int and String"

 , "bool < int" ~: evalFail "fals < 4" "Cannot compare Bool and Int"

 , "int < bool" ~: evalFail "4 < fals" "Cannot compare Int and Bool"

 , "str < float" ~: evalFail "\"abc\" < 4.2" "Cannot compare String and Float"

 , "float < str" ~: evalFail "4.2 < \"abc\"" "Cannot compare Float and String"

 , "bool < float" ~: evalFail "fals < 4.2" "Cannot compare Bool and Float"

 , "float < bool" ~: evalFail "4.2 < fals" "Cannot compare Float and Bool"

 , "bool < str" ~: evalFail "fals < \"abc\"" "Cannot compare Bool and String"

 , "str < bool" ~: evalFail "\"abc\" < fals" "Cannot compare String and Bool"

 , "bool < bool" ~: evalFail "adevarat < fals" "Cannot compare Bools"

 , "int < int" ~:
    "adevarat" ~=? evalTest "1 < 23"

 , "int < float" ~:
    "adevarat" ~=? evalTest "1 < 1.4"

 , "float < int" ~:
    "adevarat" ~=? evalTest "1.0 < 3"

 , "float < float" ~:
    "adevarat" ~=? evalTest "1.3 < 1.4"

 , "str < str" ~:
    "adevarat" ~=? evalTest "\"ana are mere\" < \"ana are pere\""

 , "str % int" ~: evalFail "\"abc\" % 4" "Modulo cannot be applied to String and Int"

 , "int % str" ~: evalFail "4 % \"abc\"" "Modulo cannot be applied to Int and String"

 , "bool % int" ~: evalFail "fals % 4" "Modulo cannot be applied to Bool and Int"

 , "int % bool" ~: evalFail "4 % fals" "Modulo cannot be applied to Int and Bool"

 , "str % float" ~: evalFail "\"abc\" % 4.2" "Modulo cannot be applied to String and Float"

 , "float % str" ~: evalFail "4.2 % \"abc\"" "Modulo cannot be applied to Float and String"

 , "bool % float" ~: evalFail "fals % 4.2" "Modulo cannot be applied to Bool and Float"

 , "float % bool" ~: evalFail "4.2 % fals" "Modulo cannot be applied to Float and Bool"

 , "bool % str" ~: evalFail "fals % \"abc\"" "Modulo cannot be applied to Bool and String"

 , "str % bool" ~: evalFail "\"abc\" % fals" "Modulo cannot be applied to String and Bool"

 , "bool % bool" ~: evalFail "adevarat % fals" "Modulo cannot be applied to Bools"

 , "float % float" ~: evalFail "2.3 % 2.4" "Modulo cannot be applied to Floats"

 , "str % str" ~: evalFail "\"abc\" % \"cba\"" "Modulo cannot be applied to Strings"

 , "int % float" ~: evalFail "2 % 2.4" "Modulo cannot be applied to Int and Float"

 , "float % int" ~: evalFail "2.3 % 2" "Modulo cannot be applied to Float and Int"

 , "str / int" ~: evalFail "\"abc\" / 4" "Cannot divide String by Int"

 , "int / str" ~: evalFail "4 / \"abc\"" "Cannot divide Int by String"

 , "bool / int" ~: evalFail "fals / 4" "Cannot divide Bool by Int"

 , "int / bool" ~: evalFail "4 / fals" "Cannot divide Int by Bool"

 , "str / float" ~: evalFail "\"abc\" / 4.2" "Cannot divide String by Float"

 , "float / str" ~: evalFail "4.2 / \"abc\"" "Cannot divide Float by String"

 , "bool / float" ~: evalFail "fals / 4.2" "Cannot divide Bool by Float"

 , "float / bool" ~: evalFail "4.2 / fals" "Cannot divide Float by Bool"

 , "bool / str" ~: evalFail "fals / \"abc\"" "Cannot divide Bool by String"

 , "str / bool" ~: evalFail "\"abc\" / fals" "Cannot divide String by Bool"

 , "bool / bool" ~: evalFail "adevarat / fals" "Cannot divide Bools"

 , "float / float" ~:
    "adevarat" ~=? evalTest "2.3 / 2.4 > 0.95 si 2.3 / 2.4 < 0.96"

 , "str / str" ~: evalFail "\"abc\" / \"cba\"" "Cannot divide Strings"

 , "int / float" ~:
    "adevarat" ~=? evalTest "2 / 2.4 > 0.83 si 2/2.4 < 0.84"

 , "float / int" ~:
    "1.15" ~=? evalTest "2.3 / 2"

 , "str * int" ~: evalFail "\"abc\" * 4" "Cannot multiply String and Int"

 , "int * str" ~: evalFail "4 * \"abc\"" "Cannot multiply Int and String"

 , "bool * int" ~: evalFail "fals * 4" "Cannot multiply Bool and Int"

 , "int * bool" ~: evalFail "4 * fals" "Cannot multiply Int and Bool"

 , "str * float" ~: evalFail "\"abc\" * 4.2" "Cannot multiply String and Float"

 , "float * str" ~: evalFail "4.2 * \"abc\"" "Cannot multiply Float and String"

 , "bool * float" ~: evalFail "fals * 4.2" "Cannot multiply Bool and Float"

 , "float * bool" ~: evalFail "4.2 * fals" "Cannot multiply Float and Bool"

 , "bool * str" ~: evalFail "fals * \"abc\"" "Cannot multiply Bool and String"

 , "str * bool" ~: evalFail "\"abc\" * fals" "Cannot multiply String and Bool"

 , "bool * bool" ~: evalFail "adevarat * fals" "Cannot multiply Bools"

 , "float * float" ~:
    "5.52" ~=? evalTest "2.3 * 2.4"

 , "str * str" ~: evalFail "\"abc\" * \"cba\"" "Cannot multiply Strings"

 , "int * float" ~:
    "4.8" ~=? evalTest "2 * 2.4"

 , "float * int" ~:
    "4.6" ~=? evalTest "2.3 * 2"

 , "str - int" ~: evalFail "\"abc\" - 4" "Cannot subtract Int from String"

 , "int - str" ~: evalFail "4 - \"abc\"" "Cannot subtract String from Int"

 , "bool - int" ~: evalFail "fals - 4" "Cannot subtract Int from Bool"

 , "int - bool" ~: evalFail "4 - fals" "Cannot subtract Bool from Int"

 , "str - float" ~: evalFail "\"abc\" - 4.2" "Cannot subtract Float from String"

 , "float - str" ~: evalFail "4.2 - \"abc\"" "Cannot subtract String from Float"

 , "bool - float" ~: evalFail "fals - 4.2" "Cannot subtract Float from Bool"

 , "float - bool" ~: evalFail "4.2 - fals" "Cannot subtract Bool from Float"

 , "bool - str" ~: evalFail "fals - \"abc\"" "Cannot subtract String from Bool"

 , "str - bool" ~: evalFail "\"abc\" - fals" "Cannot subtract Bool from String"

 , "bool - bool" ~: evalFail "adevarat - fals" "Cannot subtract Bools"

 , "float - float" ~:
    "adevarat" ~=? evalTest "2.3 - 2.4 < 0 si 2.3 - 2.4 > -0.5"

 , "str - str" ~:
    "\"bca\"" ~=? evalTest "\"bcabca\" - \"abc\""

 , "int - float" ~:
    "adevarat" ~=? evalTest "2 - 2.4 < 0 si 2 - 2.4 > -0.5"

 , "float - int" ~:
    "adevarat" ~=? evalTest "2.3 - 2 > 0.2 si 2.3 - 2 < 0.4"

 , "str + int" ~:
    "\"abc4\"" ~=? evalTest "\"abc\" + 4"

 , "int + str" ~:
    "\"4abc\"" ~=? evalTest "4 + \"abc\""

 , "bool + int" ~: evalFail "fals + 4" "Cannot add together Bool and Int"

 , "int + bool" ~: evalFail "4 + fals" "Cannot add together Int and Bool"

 , "str + float" ~:
    "\"abc4.2\"" ~=? evalTest "\"abc\" + 4.2"

 , "float + str" ~:
    "\"4.2abc\"" ~=? evalTest "4.2 + \"abc\""

 , "bool + float" ~: evalFail "fals + 4.2" "Cannot add together Bool and Float"

 , "float + bool" ~: evalFail "4.2 + fals" "Cannot add together Float and Bool"

 , "bool + str" ~:
    "\"adevaratabc\"" ~=? evalTest "adevarat + \"abc\""

 , "bool_false + str" ~:
    "\"falsabc\"" ~=? evalTest "fals + \"abc\""

 , "str + bool" ~:
    "\"abcfals\"" ~=? evalTest "\"abc\" + fals"

 , "str + bool_true" ~:
    "\"abcadevarat\"" ~=? evalTest "\"abc\" + adevarat"

 , "bool + bool" ~: evalFail "adevarat + fals" "Cannot add together Bools"

 , "float + float" ~:
    "adevarat" ~=? evalTest "2.3 + 2.4 > 4.6 si 2.3+2.4<4.8"

 , "str + str" ~:
    "\"abccba\"" ~=? evalTest "\"abc\" + \"cba\""

 , "int + float" ~:
    "4.4" ~=? evalTest "2 + 2.4"

 , "float + int" ~:
    "4.3" ~=? evalTest "2.3 + 2"
  ]
