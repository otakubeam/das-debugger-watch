options indenting = 4
options no_unused_block_arguments = false
options no_unused_function_arguments = false
options no_aot = true
options strict_smart_pointers = true

require dastest/testing_boost
require peg/peg


def parse_expr(calc: string)
    parse calc
        var add: int

        rule(add as a, "+", mul as m) <|
            return a + m
        rule(add as a, "-", mul as m) <|
            return a - m
        rule(mul as m) <|
            return m

        var mul: int

        rule(mul as m, "*", unary as u) <|
            return m * u
        rule(mul as m, "/", unary as u) <|
            return m / u
        rule(unary as u) <|
            return u

        var unary: int

        rule("-", unary as u) <|
            return u
        rule(prim as p) <|
            return p

        var prim: int

        rule("(", add as a, ")") <|
            return a
        rule(number as n) <|
            return n


[test]
def test_calc_1(t: T?)
    let input = "1+2*2+1---(3*2+1*2)-1-1-1*2"
    var res <- parse_expr(input)
    t |> equal(-6, res.value)


[test]
def test_calc_2(t: T?)
    let input = "5-2*3+4/2"
    var res <- parse_expr(input)
    t |> equal(1, res.value)


[test]
def test_calc_3(t: T?)
    let input = "4*(2+3)-6"
    var res <- parse_expr(input)
    t |> equal(14, res.value)


[test]
def test_calc_4(t: T?)
    let input = "(2+3)*4-6"
    var res <- parse_expr(input)
    t |> equal(14, res.value)


[test]
def test_calc_5(t: T?)
    let input = "2+3*4-6"
    var res <- parse_expr(input)
    t |> equal(8, res.value)


[test]
def test_calc_6(t: T?)
    let input = "10/2+5*2"
    var res <- parse_expr(input)
    t |> equal(15, res.value)


[test]
def test_calc_7(t: T?)
    let input = "(10-4)/2+5"
    var res <- parse_expr(input)
    t |> equal(8, res.value)


[test]
def test_calc_8(t: T?)
    let input = "10-4/(2+3)"
    var res <- parse_expr(input)
    t |> equal(10, res.value)
