
require parser_generator public

require strings

struct Parser
    add_cache: table<int; Result>

    input: string
    index: int


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


typedef Result = tuple<success:bool; value:int; endpos:int>;


def move(var parser: Parser; offset : int) 
    parser.index += offset


[export]
def main
    print("Compiles")
    
    var parser <- [[Parser]]
    parser.input = "1+1+1+1"
    
    var result <- parser |> parse_add