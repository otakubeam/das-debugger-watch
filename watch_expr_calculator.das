require strings public
require daslib/match public


struct Position
    row: int 
    column: int


variant Token
    plus: void
    minus: void

    star: void
    slash: void

    num: int
    name: string
    

struct CalcParser
    // Array of input charachters
    input: string
    
    // Index into the input
    index: uint

    // Current position
    pos: Position



variant AstContents
    add: tuple<AstNode?; AstNode?>
    sub: tuple<AstNode?; AstNode?>

    mul: tuple<AstNode?; AstNode?>
    div: tuple<AstNode?; AstNode?>

    literal: LiteralNode


struct AstNode
    pos: Position
    contents: AstContents



variant LiteralNode
    tok: Token



def consume_next_char(var parser: CalcParser)
    parser.index =+ 1


// Precondition: the index at position must be a digit
def lex_number(var parser: CalcParser): int
    var char = parser |> peek_next_char()
    var result = 0

    while char |> is_number()
        result *= 10
        result += char

        parser |> consume_next_char()
        char = parser |> peek_next_char()

    return result


def peek_next_char(var parser: CalcParser): int
    var idx = parser.index
    return parser.input[idx]


def position_move_right(var parser: CalcParser)
    parser |> consume_next_char()
    parser.pos.column += 1


def position_move_line(var parser: CalcParser)
    parser |> consume_next_char()
    parser.pos.row += 1


def get_current_token(var parser: CalcParser): Token
    while true

        var ch = parser |> peek_next_char()

        if is_new_line(ch)
            parser |> position_move_line()
        elif is_white_space(ch) // ' ' & '\t'
            parser |> position_move_right()

        if ch == '-'
            return [[Token minus = void]]
        // if ch == '+'
        //     return [[Token plus]]
        // if ch == '*'
        //     return [[Token star]]
        // if ch == '/'
        //     return [[Token slash]]

        if _
            if ch |> is_number()
                return [[Token num = lex_number()]]


// def parse_multiplication(var parser: CalcParser): AstNode?
//     var result : AstNode? // Can I cast nodes and references to each other?
    
//     result = parser |> parse_multiplication()

//     while true // match while next_token is either + or
//         var token = parser |> get_current_token()

//         var right = parser |> parse_multiplication()
        
//         match token
//             if _ as plus

//                 result = new [[AstNode add = [[auto result, right]] ]]
//             if _ as minus
//                 result = new [[AstNode sub = [[auto result, right]] ]]
//             if _
//                 break
    
//     return result
 

// def parse_addition(var parser: CalcParser): AstNode?
//     var result : AstNode? // Can I cast nodes and references to each other?

//     result = parser |> parse_multiplication()

//     while true // match while next_token is either + or -
//         var token = parser |> get_current_token()

//         var right = parser |> parse_multiplication()
        
//         match token
//             if _ as plus
//                 result = new [[AstNode add = [[auto result, right]] ]]
//             if _ as minus
//                 result = new [[AstNode sub = [[auto result, right]] ]]
//             if _
//                 break
    
//     return result


// def parse_expression(var parser: CalcParser): AstNode?
//     return parser |> parse_addition()
        

[export]
def main
    var parser: CalcParser = [[CalcParser input =  "1234"]]

    let n = parser |> lex_number()

    print("{n}\n")

    return 0
