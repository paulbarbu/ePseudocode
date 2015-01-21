#! /bin/sh

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


# load shunit2
. test/shell/shunit2
