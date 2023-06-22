options debugger
require daslib/debug
require strings_boost public


struct Position
    row: int 
    column: int


enum Token 
    plus
    minus
    slash
    star


struct CalcParser
    // Array of input charachters
    input: string
    
    // Index into the input
    index: uint

    // Current position
    pos: Position


struct AstNode
    pos: Position
    contents: AstContents


variant AstContents
    add: tuple<AstNode?; AstNode?>
    sub: tuple<AstNode?; AstNode?>

    mul: tuple<AstNode?; AstNode?>
    div: tuple<AstNode?; AstNode?>

    literal: LiteralNode


variant LiteralNode
    integer: int
    str: string


// Precondition: the index at position must be a digit
def lex_number(var parser: CalcParser): int
    var char = parser |> peek_next_char()
    var result = 0

    while char |> is_digit()
        result *= 10
        result += char

        parser |> consume_next_char()
        char = parser |> peek_next_char()

    return result


def peek_next_char(var parser: CalcParser): int
    var idx = parser.index
    return parser.input[idx]


def position_move_right(var parser: CalcParser)
    parser.index =+ 1
    parser.pos.col += 1


def position_move_line(var parser: CalcParser)
    parser.pos.row += 1


def get_current_token(var parser: CalcParser): Token
    while true

        var ch = parser |> peek_next_char()

        match ch
            if ch == '\n'
                parser |> position_move_line()

            if is_whitespace(ch) // ' ' & '\t'
                parser |> position_move_right()

            if ch == '-'
                return [[Token minus]]
            if ch == '+'
                return [[Token plus]]
            if ch == '*'
                return [[Token star]]
            if ch == '/'
                return [[Token slash]]

            if _
                if ch |> is_digit()
                    return [[Token ]]


def parse_multiplication(var parser: CalcParser): AstNode?
    var result : AstNode? // Can I cast nodes and references to each other?
    
    result = parser |> parse_multiplication()._0

    while true // match while next_token is either + or
        var token = parser |> get_current_token()

        var right = parser |> parse_multiplication()._0
        
        match token
            if Token plus
                result = new [[AstNode add [[auto result, right]] ]]
            if Token minus
                result = new [[AstNode sub [[auto result, right]] ]]
            if _
                break
    
    return result
 

def parse_expression(var parser: CalcParser): AstNode?

    def parse_addition(var parser: CalcParser): AstNode?
        var result : AstNode? // Can I cast nodes and references to each other?

        result = parser |> parse_multiplication()._0

        while true // match while next_token is either + or
            var token = parser |> get_current_token()

            var right = parser |> parse_multiplication()._0
            
            match token
                if Token plus
                    result = new [[AstNode add [[auto result, right]] ]]
                if Token minus
                    result = new [[AstNode sub [[auto result, right]] ]]
                if _
                    break
        
        return result
    
    return parser |> parse_addition()
        

[export]
def main
    "Hello world\n" |> print()
