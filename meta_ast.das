require daslib/match public
require daslib/fio
require daslib/rtti
require daslib/ast
require daslib/ast_boost
require daslib/templates_boost


struct Grammar
    defs: array<Definition>


// E.g. rule_name: rule
struct Definition
    name: string
    alts: array<Alternative> // Here trying to build the grammar into data strucutres (it doesn't work like this)


struct Alternative
    alternatives: array<Rule?>
     

variant Rule
    seq: array<Rule?>   // R1 R2
    alt: Alternative    // R1 | R2
    
    maybe_repeat: Rule? // Rule*
    repeat: Rule?       // Rule+

    option: Rule?       // Rule?

    nonterminal: string
    terminal: Terminal 

    // Do not consume the input
    not: Rule?     // !Rule
    and: Rule?     // &Rule

            

def abort(message: string)
    print("{message}\n")
    unsafe { exit(1); }
      

def iota
    return <- generator<int>() <| 
        var i: int = 0
        while true
            i += 1
            return false if i == 0 else yield i    


def generate(rule : Rule): auto // some AST for this rule
    match rule
        if [[Rule seq = rules]]

            var gen: array<auto> 

            for rule, i in rules, iota()
                gen |> push <| generate(rule?)

            // Turn gen array of AST block pointers to AST block pointer

            return 

        if [[Rule alt = alts]]

            var gen: array<auto> 

            for alt, i in alts.alternatives, iota()
                var block_name = "blk" + string(i)
                var parse_pos = "pos" + string(i)
                var result = "res" + string(i)

                gen |> push <| qmacro_block() <|
                    // "Alternative {i}: {rule}\n"
                                
                    parser |> reset(mark)
                    var parse_pos <- parser |> mark

                    var $i(block_name) <- $ <|
                        generate(alt?)         // Here I should expand the result somehow
                    
                    var $i(result) <- $i(block_name) |> invoke()

                    return $i(result) if $i(result).success

                    parser |> reset(parse_pos)
                
                // Turn gen array of AST block pointers to AST block pointer

        if _
            abort("TODO: more rules")


variant Terminal
    // Literal: "Hello world"
    lit: string 

    // Range: [0-9]
    range_: tuple<int; int> 



def generate(var terminal: Terminal): auto // AST

    var input: string

    match terminal 
        if [[Terminal lit = $v(l) ]]

            var q <- qmacro_block() <| $
                return [[Result]] if !parser |> matches($v(l))
            
            return q

        if [[Terminal range_ = $v(r) ]]
            abort("TODO: ranges")

        if _
            abort("")

    // TODO: remove
    return [[ExpressionPtr]]



[export]
def main
    print("\n")
    
    generate([[Terminal lit = "123"]]) |> describe() |> print()
    
    for x,i,_ in  iota(), iota(), range(10)
        print("{x}, {i}\n")
