require strings
require daslib/json public

// Grammar
//
// start:   int -> add 
// mul:     int -> mul '*' add 
//                | add
// add:     int -> add '+' NUM
//                | primary
// primary: int -> NUM | '(' add ')'
// NUM:     int -> '1'
//

struct Parser
    add_cache: table<int; Result>

    input: array<uint8>
    index: int


def mark(parser: Parser): int
    return parser.index


def reset(var parser: Parser; pos: int)
    parser.index = pos


def matches(var parser: Parser; template_: string; strlen: int): bool
    var mark <- parser |> mark

    // Not sufficient supply of charachters in the input
    return false if parser.input |> length < mark + strlen

    for i in range(strlen)
        var template_char = unsafe(template_ |> character_uat(i) ) |> uint8()
        var parser_input = parser.input[i + mark]
        return false if template_char != parser_input

    parser |> move(strlen)
    return true


typedef Result = tuple<success:bool; value:int; endpos:int>;


def move(var parser: Parser; offset: int)
    parser.index += offset


def parse_num(var parser: Parser): Result
    return [[Result]] if !parser |> matches("1", "1" |> length)

    var mark <- parser |> mark

    return [[auto true, 1, mark]]

    
def parse_primary(var parser: Parser): Result

    // Alternative 1: NUM

    var mark <- parser |> mark
    var res <- parser |> parse_num

    return res if res.success

    parser |> reset(mark)
    
    // Alternative 2: (NUM)
    
    var pos_1 <- parser |> mark

    var blk = $ <| 
        return [[Result]] if !parser |> matches("(", 1)

        var v1 <- parser |> parse_add
        return [[Result]] if !v1.success
        
        return [[Result]] if !parser |> matches(")", 1)

        // Here goes action perhaps?
        return [[Result true, v1.value, parser |> mark]]

    res <- invoke(blk)

    return res if res.success

    parser |> reset(pos_1)
    
    // Default
    
    return [[Result]]
 
    
def parse_add(var parser: Parser): Result
    var mark = parser |> mark
    
    with parser 
        if add_cache |> key_exists(index)
            var result = add_cache[index]

            parser |> reset(result.endpos)
            return result

        // Build new cache entry from scratch
        
        var res = [[Result]]
        add_cache[mark] = res

        while true
            parser |> reset(mark)

            var newres = parser |> parse_add_inner
            var endpos = parser |> mark

            // Break if no movement
            break if res.endpos >= endpos

            res = newres
            add_cache[mark] = res

        parser |> reset <| res.endpos
        return res
            
    
def parse_add_inner(var parser: Parser): Result
    with parser
        // Alternative 1: add '+' primary

        var pos_1 <- parser |> mark

        var blk = $ <| 
            var add_1 <- parser |> parse_add
            return [[Result]] if !add_1.success
                
            return [[Result]] if !parser |> matches("+", 1)

            var prim_1 <- parser |> parse_primary
            return [[Result]] if !prim_1.success

            // Here goes action perhaps?
            var val <- add_1.value + prim_1.value
            
            var mark <- parser |> mark
            return [[Result true, val, mark]]

        var res <- invoke(blk)

        return res if res.success
            
        parser |> reset(pos_1)
        
        // Alternative 2: primary
        
        pos_1 = parser |> mark

        var blk2 <- $ <| 
            var prim_1 <- parser |> parse_primary
            return [[Result]] if !prim_1.success

            // Here goes action perhaps?
            return prim_1

        res <- invoke(blk2)

        return res if res.success

        parser |> reset(pos_1)
        
        // Default
        
        return [[Result]]

[export]
def main
    var parser <- [[Parser]]
    // parser.input = array<uint8>("1+1+1+1")

    for ch in "1+1+1"
        parser.input |> push <| (ch |> uint8())
    
    var result <- parser |> parse_add
    
    print("{parser} => {result}\n")
