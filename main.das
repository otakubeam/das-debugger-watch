options indenting = 4
options no_unused_block_arguments = false
options no_unused_function_arguments = false
options no_aot = true
options strict_smart_pointers = true

require parser_generator public

// options log_infer_passes

require strings
require daslib/strings_boost

options debugger
require daslib/debug

require parse_macro


struct Parser
    prim_cache: table<int; Result>
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


def skip_whitespace(var parser: Parser)
    while true
        var ch = parser |> get_current_char
        return if !ch |> is_white_space
        parser |> move(1)


def match_string_literal(var parser: Parser): tuple<success:bool; string>
    //! Tries to match everything inside ""

    // Define buffer to store string literal characters
    var inscope buffer: das_string

    var current_char = parser |> get_current_char

    // If the current character is not a double quote, the rule is not a string
    return [[auto false, ""]] if current_char != '"'
    
    parser |> move(1)
    current_char = parser |> get_current_char

    while current_char != '"' && current_char != -1
        buffer |> append(current_char)
        parser |> move(1)
        current_char = parser |> get_current_char

    // If we've reached EOF file without finding a closing quote
    return [[auto false, ""]] if current_char == -1

    parser |> move(1)
    return [[auto true, buffer |> string()]]


def match_double_literal(var parser: Parser): tuple<success:bool, double>
    //! Matches doubles in the form of [-+]? [0-9]* .? [0-9]+ ([eE] [-+]? [0-9]+)?
    //! The number is not checked to be representable as defined in IEEE-754

    var inscope buffer: das_string
    var current_char = parser |> get_current_char

    if current_char == '-' || current_char == '+'
        buffer |> append(current_char)
        parser |> move(1)
        current_char = parser |> get_current_char

    // Add everything up to '.' to the buffer
    while current_char |> is_number || current_char == '.'
        buffer |> append(current_char)
        parser |> move(1)
        current_char = parser |> get_current_char
    

    // Match exponent part
    if current_char == 'e' || current_char == 'E'
        buffer |> append(current_char)
        parser |> move(1)
        current_char = parser |> get_current_char

        // Check for '-' or '+' after 'e' or 'E'
        if current_char == '-' || current_char == '+'
            buffer |> append(current_char)
            parser |> move(1)
            current_char = parser |> get_current_char

        // Continue appending digits after 'e' or 'E'
        while current_char |> is_number
            buffer |> append(current_char)
            parser |> move(1)
            current_char = parser |> get_current_char
 
    return [[auto true, buffer |> string() |> double()]]


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


def reached_EOF(var parser: Parser): bool 
    return parser |> get_current_char != -1


def move(var parser: Parser; offset : int) 
    parser.index += offset


[export]
def main

    var parser <- [[Parser]]
    parser.input = "1+2*2+1---(3*2+1*2)-1-1-1*2"

    parse calc
        let add: int

        rule(add as a, "+", mul as m) <|
            return a + m
        rule(add as a, "-", mul as m) <|
            return a - m
        rule(mul as m) <|
            return m

        let mul: int

        rule(mul as m, "*", unary as u) <|
            return m * u 
        rule(mul as m, "/", unary as u) <|
            return m / u 
        rule(unary as u) <|
            return u 

        let unary: int
        
        rule("-", unary as u) <|
            return u 
        rule(prim as p) <|
            return p 

        let prim: int
        
        rule("(", add as a, ")") <|
            return a
        rule(number as n) <| 
            return n

    var res <- parser |> parse_add

    print("Assert {res.value} == -6\n")
