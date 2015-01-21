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

# load shunit2
. test/shell/shunit2
