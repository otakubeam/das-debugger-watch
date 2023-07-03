
require parser_generator public

//options log_infer_passes

require strings
require daslib/strings_boost

options debugger
require daslib/debug

require parse_macro


struct Parser
    primary_cache: table<int; Result>
    unary_cache: table<int; Result>
    mul_cache: table<int; Result>
    add_cache: table<int; Result>

    input: string
    index: int


def get_current_char(var parser: Parser): int
    return -1 if parser.input |> length == parser.index 
    return parser.input |> character_at(parser.index)


def lex_number(var parser: Parser): Result
    return [[Result]] if !parser |> get_current_char |> is_number
    
    var result = 0

    while parser |> get_current_char |> is_number()
        result *= 10
        result +=  parser |> get_current_char - '0'
        parser |> move(1)

    return [[Result true, result, parser |> mark]]


def mark(parser: Parser): int
    return parser.index


def reset(var parser: Parser; pos: int)
    parser.index = pos


def matches(var parser: Parser; template_: string): bool
    with parser
        var st = input |> slice(index)
        if !st |> starts_with(template_)
            return false
        parser |> move(template_ |> length)
        return true


def move(var parser: Parser; offset : int) 
    parser.index += offset


[export]
def main

    var parser <- [[Parser]]
    parser.input = "1+2*2+1---(3*2+1*2)-1-1-1*2"
    let calc: int

    parse calc
        // let add: int

        // rule(add as a, "+", mul as m) <|
        //     return 1 + 2
        // rule(add as a, "-", mul as m) <|
        //     return a - m
        // rule(mul as m) <|
        //     return m

        // let mul: int

        // rule(mul as m, "*", prim as p) <|
        //     return m * p 
        // rule(mul as m, "/", prim as p) <|
        //     return m / p 
        // rule(prim as p) <|
        //     return p 

        let prim: int
        
        // rule("(", add as a, ")") <|
        //     return a

        rule(number) <| 
            return 1

    var res <- parser |> parse_prim

    print("{res.value} == -6 ?\n")