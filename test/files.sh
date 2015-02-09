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
    assertTrue 'Expression instead of main' 'grep -q "expecting \"func\", identifier or \"struct\""<<<"$(interpreter test/epc/expr.epc)"'
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

testClosure()
{
    assertEquals "$(echo -e '8\n4')" "$(interpreter examples/closure.epc)"
}

testStructs()
{
out1=$(cat <<'EOOUT'
p.x = 0
p.x = 3
p.x = 1 p.y = 1
translate was called
p.x = 3 p.y = 3
s.str[1] = o
s.str[1] = p
s.str = fpobarbaz
s.elems[2] = <user defined type>
s.elems[2] = 42
s.elems = {1, 2, 42}
s.elems = {}
l[1].x = 0
l[1].x = 3
l[1].elems[1] = 2
l[1].elems[1] = 3
l[1].elems[2].x = 0
l[1].elems[2].x = 2 l[1].elems[2].y = 2
translate was called
l[1].elems[2].x = 3 l[1].elems[2].y = 1
dp.dpoint.x = 0
dp.x = 2 dp.y = 1
EOOUT
)

out2=$(cat <<'EOOUT'
x=123 y=0
translate was called
x=124 y=1
EOOUT
)
    assertEquals "$out1" "$(interpreter test/epc/struct.epc)"
    assertEquals "$out2" "$(interpreter examples/user_def_types.epc)"
}

testHuffman()
{
out=$(cat <<'EOOUT'
1:f -- 0
2:o -- 1
EOOUT
)
    assertEquals "$out" "$(interpreter examples/huffman.epc foo)"
}

# load shunit2
. test/shell/shunit2
