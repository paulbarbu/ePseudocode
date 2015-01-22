#! /bin/bash

#helper function to call my interpreter with arguments
interpreter()
{
    dist/build/epseudocode/epseudocode "$@"
}

testArgv()
{
    assertEquals '{"1", "2", "3"}' "$(interpreter examples/argv.epc 1 2 3)"
    assertEquals '{"42"}' "$(interpreter examples/argv.epc 42)"
}

testFib()
{
    assertEquals "55" "$(interpreter examples/fib.epc)"
}

testFor()
{
    assertEquals "Sum = 5050" "$(interpreter examples/for.epc)"
}

testGlobal()
{
    assertEquals "42" "$(interpreter examples/global.epc)"
}

testHello()
{
    assertEquals "Hello world" "$(interpreter examples/hello.epc)"
}

testDoubleFunctionDef()
{
    assertEquals 'Error: The function name "a" shadows another name in the current scope' "$(interpreter test/epc/double_function_def.epc)"
}

testExprNoMain()
{
    assertTrue 'Expression insted of main' 'grep -q "expecting \"func\" or identifier"<<<"$(interpreter test/epc/expr.epc)"'
}

testNoMainFunc()
{
    assertEquals 'Error: Unbound variable name main' "$(interpreter test/epc/no_main.epc)"
}

testFizzBuzz()
{
out=$(cat <<'EOOUT'
n=1
2
fizz
4
buzz
fizz
7
8
fizz
buzz
11
fizz
13
14
fizzbuzz
EOOUT
)
    assertEquals "$out" "$(echo 15 | interpreter examples/fizzbuzz.epc)"
}

testLists()
{
out=$(cat <<'EOOUT'
0:1
1:2
2:5
3:7
4:8
EOOUT
)
    assertEquals "$out" "$(interpreter examples/lists.epc)"
}

testGcd()
{
    assertEquals "$(echo -e '5\n1')" "$(interpreter examples/greatest_common_div.epc)"
}

testCallback()
{
    assertEquals "246810" "$(interpreter examples/callback.epc)"
}

# load shunit2
. test/shell/shunit2
