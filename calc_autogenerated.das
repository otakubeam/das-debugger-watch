require strings
require daslib/match public

// Grammar
//
// start: int -> add 
// add: int -> add '+' NUM
//      | primary
// primary: int -> NUM | '(' add ')'
// NUM: int -> '1'
//

struct Parser
    add_cache: table<int; void?>

    input: string
    index: int


def mark(parser: Parser): int
    return parser.index


def reset(var parser: Parser; pos: int)
    parser.index = pos


def expect(var parser: Parser; template: string): bool
    // skip_whitespace()

    with parser
        var st = input |> chop(0, index)
        return st |> starts_with(template)
        


def parse_add(var parser: Parser): int
    with parser

        if false // has index in add_cache
            return add_cache[index]
        
        // Parse the first alternative: add '+' primary

        var pos_1 <- parser |> mark()

        var blk = $ <| 
            var add_1 <- parser |> parse_add

            return null if add_1 == null
            return null if !paresr |> expect("+")

            var prim_1 <- parser |> parse_primary
            return null if prim_1 == null

            // Here goes action perhaps?
            return [[Node add = {add_1, prim_1}]]

        var res <- invoke(blk)

        if res != null
            return res

        parser |> reset(pos_1)

        // Parse the second alternative: primary
        
        var pos_1 <- parser |> mark()

        var blk = $ <| 
            var prim_1 <- parser |> parse_primary
            return null if prim_1 == null

            // Here goes action perhaps?
            return [[Node add = {add_1, prim_1}]]

        var res <- invoke(blk)

        if res != null
            return res

        parser |> reset(pos_1)
    
    return null


def parse_primary(var parser: Parser): void?


def parse_num(var paresr: Parser): void?
